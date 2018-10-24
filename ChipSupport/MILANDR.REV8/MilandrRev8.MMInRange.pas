unit MilandrRev8.MMInRange;

interface
  uses
  AbstractStpMethod, MilandrRev8.Stp8TestThread, MilandrRev8.DboBase,
  MilandrRev8IMS, ToleranceObj, System.Generics.Collections,
  VdpDMAccess, System.IniFiles, Stp8PositionDBExtention,
  ChipAbstractInterface, System.Generics.Defaults;

  type
  TMilRev8InRange = class(TAbstractStpMethod)
   protected
    function ThreadClass : TStpThreadClass; override;
   public
    function ModuleName : string; override;
    function ReadyToStart:Boolean; override;
  end;

  TCharactItem = class(TMilandrRev8Registers)
  public
   Step : Byte;
   CharactBitValue : SmallInt;
  end;

  TCharactPattern = class(TObject)
   private
    fCharactBitsIdx : TChipBytes;
    fList : TObjectList<TCharactItem>;
   public
    ID : Integer;
    constructor Create;
    destructor Destroy;override;
    procedure ReadPattern(const ADM: IDMAccess; const AConnection : TvdConnection; AID: integer);
    property List : TObjectList<TCharactItem> read fList;
  end;

  TVFPattern = class(TObject)
   private
    fList : TObjectList<TMilandrRev8Registers>;
   public
    ID : Integer;
    constructor Create;
    destructor Destroy;override;
    procedure ReadPattern(const ADM: IDMAccess; const AConnection : TvdConnection; AID: integer);
    property List : TObjectList<TMilandrRev8Registers> read fList;
  end;

  TMilRev8InRangeThread = class(TMilRev8AbstractTestThread)
   private
    fTestSeqence : TInRangeProcOption;
    fPointCharactPatterns : TObjectList<TCharactPattern>;
    fVFPatterns : TObjectList<TVFPattern>;
    fFCRegisters : TChipBytes;
    procedure StartMessage;
    procedure FinishMessage;
   protected
    function PositionClass : TProcessedPositionClass;override;
    function PreparePosition(const APosition : TProcessedPosition):Boolean; override;
    procedure StartMain; override;
    function VvarCurrMeasFunct: Boolean;
    function FreqCurrMeasFunct: Boolean;
    function FreqFinalMeasFunct: Boolean;
    function FreqZeroMeasFunct: Boolean;
    function VvarCharactMeasFunct: Boolean;
    function VvarFCMeasFunct: Boolean;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.Math,
  Data.DB,
  Vodopad.Math,
  Vodopad.FloatList,
  dmLoggerInterface,
  AbstractMainProcessInterface,
  AbstractBoardInterface,
  ChamberInterfaces,
  PositionListInterface,
  dmPositionControllerInterface,
  ExtentionsListInterface,
  ChipAbstract,
  MilandrRev8.Consts,
  MilandrRev8Stp8dboExtention,
  LoggerInterface,
  System.Classes;


{ TMilRev8InRange }

function TMilRev8InRange.ModuleName: string;
begin
  result := 'Milnandr rev.8 measurer';
end;

function TMilRev8InRange.ReadyToStart: Boolean;
var
vIdx, vPointIdx : word;
vTmp : IDMAccess;
vBoard : IAbstractBoard;
vChamberProcess : IChamberProcess;
vPositions : IPositionsList;
vPositionExtentions : IExtentions;
vDbExt : TStp8milDbExtention;
begin
  try
    if not (rsChecked in fReady) then
    begin
      if ((not supports(fMainProc, IDMAccess, vTmp))
      or (not supports(fBoard, IAbstractBoard, vBoard))
      or (not supports(fMainProc, IChamberProcess, vChamberProcess))
      or (not vBoard.QueryPositionListInterface(IPositionsList, vPositions))
      or (vPositions.Count < 1)) then
      begin
        vPositions := nil;
        vBoard := nil;
        vTmp := nil;
        vChamberProcess := nil;
        include(fReady, rsChecked);
        Exit;
      end;
      for vIdx := 0 to vPositions.Count-1 do
      begin
        vPositionExtentions := nil;
        vDbExt := nil;
        if ((not supports(vPositions[vIdx], IExtentions, vPositionExtentions))
        or (not vPositionExtentions.Find(TStp8milDbExtention, vDbExt))) then
          Continue;
        if (vDbExt.ItersCount > 0)
        and (vDbExt.LastIter.Options.ControlOptions.InRangeOptions[vDbExt.LastIter.Options.ControlOptions.MeasureMode].s <> [])
        and (vDbExt.LastIter.MeasPoints.Find(vChamberProcess.CurrPointTempr, vChamberProcess.MaxDeltaTempr, vPointIdx))
        and (vDbExt.LastIter.MeasPoints[vPointIdx].MeasSucc.s <>
        vDbExt.LastIter.Options.ControlOptions.InRangeOptions[vDbExt.LastIter.Options.ControlOptions.MeasureMode].s) then
        begin
          if ([rpVvarCharactMeas] = vDbExt.LastIter.Options.ControlOptions.InRangeOptions[vDbExt.LastIter.Options.ControlOptions.MeasureMode].s) then
          begin
            if vDbExt.LastIter.MeasPoints[vPointIdx].PatternExists then
               include(fReady, rsOk);
          end else if (vDbExt.LastIter.Options.ControlOptions.InRangeOptions[vDbExt.LastIter.Options.ControlOptions.MeasureMode].s = [rpFreqFinal]) then
          begin
            if vDbExt.LastIter.MeasPoints[vPointIdx].WorkPoint then
              include(fReady, rsOk);
          end else
            include(fReady, rsOk);
        end;
      end;
      include(fReady, rsChecked);
    end;
  finally
    result := (rsOk in fReady);
    vPositionExtentions := nil;
    vPositions := nil;
    vBoard := nil;
    vTmp := nil;
    vChamberProcess := nil;
  end;
end;


function TMilRev8InRange.ThreadClass: TStpThreadClass;
begin
  result := TMilRev8InRangeThread;
end;

{ TMilRev8InRangeThread }

type
TInRangePosition = class(TProcessedPosition)
  PointIdx : Word;
  function Init : boolean; virtual; abstract;
  procedure WriteMeasData(const ADB : TEmbSQLDataBase); virtual; abstract;
  procedure SaveMeasData;
end;

procedure TInRangePosition.SaveMeasData;
var
vDB : TEmbSQLDataBase;
vIterState : TIterStates;
vPosLog : TPosLogItem;
begin
  vDB := DbExt.CreateConnection;
  try
    WriteMeasData(vDB);
    if (DbExt.LastIter.MeasPoints.SuccCount = DbExt.LastIter.MeasPoints.Count) then
    begin
      DbExt.LastIter.Results.FinalizeTime := now;
      vIterState := DbExt.LastIter.State;
      Include(vIterState, isSucc);
      DbExt.LastIter.State := vIterState;
      DbExt.LastIter.Write(vDB, true, [foResults]);
      vPosLog.DT := Now;
      vPosLog.Step := Format('Итерация %d завершена',[DbExt.LastIter.Num]);
      vPosLog.Result := 'OK';
      vPosLog.Comment := '';
      vPosLog.Save(vDB);
    end;
  finally
    if vDB.TransactionActive then
      vDB.RollBack;
    vDB.DBClose;
    FreeAndNil(vDB);
  end;
end;

{$REGION 'VvarCurrMeas'}
type
TVvarCurrRawMeasureData = class(TObject)
  Registers : TMilandrRev8Registers;
  Solution:byte;
  ErrCode : TVCErrors;
  VvarValue, Tempr,
  VDD, VC : double;
  constructor Create;
  destructor Destroy;override;
end;

TVvarCurrMeasPosition = class(TInRangePosition)
 public
  CurrItem : Word;
  RawData : TObjectList<TVvarCurrRawMeasureData>;
  constructor Create(const ATestThread : TMilRev8AbstractTestThread); override;
  destructor Destroy;override;
  function NewItem : TVvarCurrRawMeasureData;
  function Init : boolean; override;
  procedure WriteMeasData(const ADB : TEmbSQLDataBase); override;
end;

constructor TVvarCurrRawMeasureData.Create;
begin
  Registers := TMilandrRev8Registers.Create(nil);
  ErrCode := [];
end;

destructor TVvarCurrRawMeasureData.Destroy;
begin
  FreeAndNil(Registers);
  inherited;
end;

constructor TVvarCurrMeasPosition.Create(const ATestThread : TMilRev8AbstractTestThread);
begin
  inherited;
  RawData := TObjectList<TVvarCurrRawMeasureData>.Create;
end;

destructor TVvarCurrMeasPosition.Destroy;
begin
  RawData.Clear;
  FreeAndNil(RawData);
  inherited;
end;

function TVvarCurrMeasPosition.NewItem : TVvarCurrRawMeasureData;
begin
  Result := TVvarCurrRawMeasureData.Create;
  RawData.Add(Result);
end;

procedure TVvarCurrMeasPosition.WriteMeasData(const ADB : TEmbSQLDataBase);
const
cInsertSqlText = 'INSERT INTO VVAR_CURR (ITER_NUM, SOL_NUM, IZM_NUM, POINTIDX, TEMPR, REGMEM, VOLTAGE, ERR_CODE, VDD, VC)'+
                                    'VALUES (:ITER_NUM, :SOL_NUM, :IZM_NUM, :POINTIDX, :TEMPR, :REGMEM, :VOLTAGE, :ERR_CODE, :VDD, :VC)';
cSelMaxIzmNumSqtText = 'SELECT MAX(IZM_NUM) FROM VVAR_CURR WHERE ITER_NUM = :ITER_NUM AND SOL_NUM = :SOL_NUM';
var
vQR : TEmbSQLRequest;
vIdx, vIzmNum : Word;
vErrAd : TVCErrorsAdapter;
vMS : TMemoryStream;
vSolNum : Byte;
vBoardLogger : IdmLogger;
begin
  vMS := TMemoryStream.Create;
  try
    ADB.TransactionBegin;
    try
      vIdx := 0;
      vSolNum := 0;
      vIzmNum := 1;
      while vIdx < RawData.Count do
      begin
        if ((vIdx = 0)
        or (vSolNum <> RawData[vIdx].Solution)) then
        begin
          vIzmNum := 1;
          vQR.Prepare(ADB.DB, cSelMaxIzmNumSqtText);
          vQR.Bind(1, DbExt.LastIter.Num);
          vQR.Bind(2, RawData[vIdx].Solution);
          if (vQR.PrepareNext = SQLEMB_DONE)
            and (vQR.Step = SQLEMB_ROW) then
            vIzmNum := vQR.FieldInt(0)+1;
          vSolNum := RawData[vIdx].Solution;
          vQR.Close;
        end;
        vMS.Clear;
        RawData[vIdx].Registers.SaveToStream(vMS);
        vMS.Position := 0;
        vQR.Prepare(ADB.DB, cInsertSqlText);
        vErrAd.i := 0;
        vErrAd.s := RawData[vIdx].ErrCode;
        vQR.Bind(1, DbExt.LastIter.Num);
        vQR.Bind(2, RawData[vIdx].Solution);
        vQR.Bind(3, vIzmNum);
        vQR.Bind(4, PointIdx);
        vQR.Bind(5, RawData[vIdx].Tempr);
        vQR.Bind(6, vMS);
        vQR.Bind(7, RawData[vIdx].VvarValue);
        vQR.Bind(8, vErrAd.i);
        vQR.Bind(9, RawData[vIdx].VDD);
        vQR.Bind(10, RawData[vIdx].VC);
        vQR.Execute;
        Inc(vIzmNum);
        Inc(vIdx);
      end;
      ADB.Commit;
      DbExt.LastIter.MeasPoints[PointIdx].MeasSuccInclude(rpVvarCurrMeas);
      DbExt.LastIter.Write(ADB, False, [foRange]);
    except on e : Exception do
      begin
        ADB.Rollback;
        if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
          vBoardLogger.Log(lWarning, Format('Exception %s "%s" on save vvar curr meas  data', [e.ClassName, e.Message]));
        vBoardLogger := nil;
      end;
    end;
  finally
    FreeAndNil(vMS);
  end;
end;

function TVvarCurrMeasPosition.Init: boolean;
var
vIdx : Word;
vItem : TVvarCurrRawMeasureData;
begin
  RawData.Clear;
  CurrItem := 0;
  vIdx := 0;
  while vIdx < DbExt.LastIter.MeasSolutions.Count do
  begin
    vItem := NewItem;
    vItem.Solution := vIdx + 1;
    vItem.Registers.Assign(DbExt.LastIter.MeasSolutions[vIdx].Registers);
    vItem.Registers.Registers.TEST := 6;
    {vItem.Registers.Registers.DA := 0;
    vItem.Registers.Registers.CLK := 1;  }
    Inc(vIdx);
  end;
  Result := (RawData.Count > 0);
  if Result then
    Chip.Assign(RawData[0].Registers);
end;
{$ENDREGION}

{$REGION 'FreqCurr'}
type

TFreqCurrRawMeasureData = class(TObject)
  Registers : TMilandrRev8Registers;
  Solution:byte;
  ErrCode : TFMErrors;
  Freq,
  DispPpm,
  Tempr,
  VDD, VC : double;
  constructor Create;
  destructor Destroy;override;
end;

TFreqCurrPosition = class(TInRangePosition)
 public
  CurrItem : Word;
  RawData : TObjectList<TFreqCurrRawMeasureData>;
  constructor Create(const ATestThread : TMilRev8AbstractTestThread); override;
  destructor Destroy;override;
  function NewItem : TFreqCurrRawMeasureData;
  function Init : boolean; override;
  procedure WriteMeasData(const ADB : TEmbSQLDataBase); override;
end;

constructor TFreqCurrRawMeasureData.Create;
begin
  Registers := TMilandrRev8Registers.Create(nil);
  ErrCode := [];
end;

destructor TFreqCurrRawMeasureData.Destroy;
begin
  FreeAndNil(Registers);
  inherited;
end;

constructor TFreqCurrPosition.Create(const ATestThread : TMilRev8AbstractTestThread);
begin
  inherited;
  RawData := TObjectList<TFreqCurrRawMeasureData>.Create;
end;

destructor TFreqCurrPosition.Destroy;
begin
  RawData.Clear;
  FreeAndNil(RawData);
  inherited;
end;

function TFreqCurrPosition.NewItem : TFreqCurrRawMeasureData;
begin
  Result := TFreqCurrRawMeasureData.Create;
  RawData.Add(Result);
end;

procedure TFreqCurrPosition.WriteMeasData(const ADB : TEmbSQLDataBase);
const
cInsertSqlText =
      'INSERT INTO FREQ_CURR (ITER_NUM, SOL_NUM, IZM_NUM, POINTIDX, TEMPR, REGMEM, FREQ, FREQDISP, ERR_CODE, VDD, VC) '+
      'VALUES (:ITER_NUM, :SOL_NUM, :IZM_NUM, :POINTIDX, :TEMPR, :REGMEM, :FREQ, :FREQDISP, :ERR_CODE, :VDD, :VC);';
cSelMaxIzmNumSqtText = 'SELECT MAX(IZM_NUM) FROM FREQ_CURR WHERE ITER_NUM = :ITER_NUM AND SOL_NUM = :SOL_NUM';
var
vQR : TEmbSQLRequest;
vIdx, vIzmNum : Word;
vErrAd : TFMErrorsAdapter;
vMS : TMemoryStream;
vSolNum : Byte;
vBoardLogger : IdmLogger;
begin
  vMS := TMemoryStream.Create;
  try
    ADB.TransactionBegin;
    try
      vIdx := 0;
      vSolNum := 0;
      vIzmNum := 1;
      while vIdx < RawData.Count do
      begin
        if ((vIdx = 0)
        or (vSolNum <> RawData[vIdx].Solution)) then
        begin
          vIzmNum := 1;
          vQR.Prepare(ADB.DB, cSelMaxIzmNumSqtText);
          vQR.Bind(1, DbExt.LastIter.Num);
          vQR.Bind(2, RawData[vIdx].Solution);
          if (vQR.PrepareNext = SQLEMB_DONE)
            and (vQR.Step = SQLEMB_ROW) then
            vIzmNum := vQR.FieldInt(0)+1;
          vSolNum := RawData[vIdx].Solution;
          vQR.Close;
        end;
        vMS.Clear;
        RawData[vIdx].Registers.SaveToStream(vMS);
        vMS.Position := 0;
        vQR.Prepare(ADB.DB, cInsertSqlText);
        vErrAd.i := 0;
        vErrAd.s := RawData[vIdx].ErrCode;
        vQR.Bind(1, DbExt.LastIter.Num);
        vQR.Bind(2, RawData[vIdx].Solution);
        vQR.Bind(3, vIzmNum);
        vQR.Bind(4, PointIdx);
        vQR.Bind(5, RawData[vIdx].Tempr);
        vQR.Bind(6, vMS);
        vQR.Bind(7, RawData[vIdx].Freq);
        vQR.Bind(8, RawData[vIdx].DispPpm);
        vQR.Bind(9, vErrAd.i);
        vQR.Bind(10, RawData[vIdx].VDD);
        vQR.Bind(11, RawData[vIdx].VC);
        vQR.Execute;
        Inc(vIzmNum);
        Inc(vIdx);
      end;
      ADB.Commit;
      DbExt.LastIter.MeasPoints[PointIdx].MeasSuccInclude(rpFreqCurr);
      DbExt.LastIter.Write(ADB, False, [foRange]);
    except
      on e : Exception do
      begin
        ADB.Rollback;
        if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
          vBoardLogger.Log(lWarning, Format('Exception %s "%s" on save freq curr meas  data', [e.ClassName, e.Message]));
        vBoardLogger := nil;
      end;
    end;
  finally
    FreeAndNil(vMS);
  end;
end;

function TFreqCurrPosition.Init: boolean;
var
vIdx : Word;
vItem : TFreqCurrRawMeasureData;
begin
  RawData.Clear;
  CurrItem := 0;
  vIdx := 0;
  while vIdx < DbExt.LastIter.MeasSolutions.Count do
  begin
    vItem := NewItem;
    vItem.Solution := vIdx + 1;
    vItem.Registers.Assign(DbExt.LastIter.MeasSolutions[vIdx].Registers);
    vItem.Registers.Registers.TEST := 0;
    {vItem.Registers.Registers.DA := 1;
    vItem.Registers.Registers.CLK := 1;}
    Inc(vIdx);
  end;
  Result := (RawData.Count > 0);
  if Result then
    Chip.Assign(RawData[0].Registers);
end;
{$ENDREGION}

{$REGION 'VvarCharact'}

constructor TCharactPattern.Create;
begin
  fList := TObjectList<TCharactItem>.Create;
  ChipAbstract.ReadControlledBitsIndex(TMilandrRev8Registers, 'Stp8ChractRegisters', False, fCharactBitsIdx);
end;

destructor TCharactPattern.Destroy;
begin
  List.Clear;
  FreeAndNil(fList);
  SetLength(fCharactBitsIdx, 0);
  inherited;
end;

procedure TCharactPattern.ReadPattern(const ADM: IDMAccess; const AConnection : TvdConnection; AID: integer);
const
C_Sel_Patterns = 'SELECT PROC_PATTERNS.DATA_TEXT '+
'FROM PROC_PATTERNS WHERE PROC_PATTERNS.PATTERN_ID = :PATTERN_ID ';
var
vQR : TvdQuery;
vSt : TStringList;
vIni : TMemIniFile;
vIdx : Word;
vItem : TCharactItem;
begin
  List.Clear;
  ID := AID;
  vQR:= ADM.CreateReadQuery(nil, C_Sel_Patterns, AConnection);
  vSt := TStringList.Create;
  vIni := TMemIniFile.Create('');
  try
    try
      vQR.Prepare;
      vQR.ParamByName('PATTERN_ID').AsInteger := AID;
      vQR.Open;
      if vQR.RecordCount > 0 then
      begin
        vSt.Clear;
        vSt.Text := vQR.FieldByName('DATA_TEXT').AsWideString;
        vIni.SetStrings(vSt);
        vSt.Clear;
        vIni.ReadSections(vST);
        vIdx := 0;
        while vIdx < vST.Count do
        begin
          if vIni.SectionExists(Format('Stp8CharactReg%d',[vIdx])) then
          begin
            vItem := TCharactItem.Create(nil);
            List.Add(vItem);
            vItem.Step := Byte(vIni.ReadInteger(Format('Stp8CharactReg%d',[vIdx]), 'Step', 0));
            vItem.LoadFromIni(vIni, Format('Stp8CharactReg%d',[vIdx]));
            vItem.CharactBitValue := vItem.BitValue[vItem.Step];
          end;
          Inc(vIdx);
        end;
        if List.Count > 0 then
        begin
          List.Sort(
            TComparer<TCharactItem>.Construct(function(const Left, Right: TCharactItem): Integer
              begin
                Result := Left.Step - Right.Step;
                if result = 0 then
                  result := Left.CharactBitValue - Right.CharactBitValue;
              end)
              );
        end;
      end;
      vQR.Close;
    except
      on e : exception do
        ADM.OnException(vQR, E);
    end;
  finally
    FreeAndNil(vQR);
    FreeAndNil(vIni);
    FreeAndNil(vSt);
  end;
end;

type
TVvarCharactRawMeasureData = class(TObject)
  Registers : TMilandrRev8Registers;
  Solution,
  vTag,
  vVar:byte;
  ErrCode : TVChErrors;
  Voltage,
  Tempr,
  VDD, VC : double;
  constructor Create;
  destructor Destroy;override;
end;

TVvarCharactMeasPosition = class(TInRangePosition)
 public
  Pattern : TCharactPattern;
  CurrItem : Word;
  RawData : TObjectList<TVvarCharactRawMeasureData>;
  constructor Create(const ATestThread : TMilRev8AbstractTestThread); override;
  destructor Destroy;override;
  function Init : boolean; override;
  function NewItem : TVvarCharactRawMeasureData;
  procedure WriteMeasData(const ADB : TEmbSQLDataBase); override;
end;

constructor TVvarCharactRawMeasureData.Create;
begin
  Registers := TMilandrRev8Registers.Create(nil);
  ErrCode := [];
end;

destructor TVvarCharactRawMeasureData.Destroy;
begin
  FreeAndNil(Registers);
  inherited;
end;

constructor TVvarCharactMeasPosition.Create(const ATestThread : TMilRev8AbstractTestThread);
begin
  inherited;
  RawData := TObjectList<TVvarCharactRawMeasureData>.Create;
end;

destructor TVvarCharactMeasPosition.Destroy;
begin
  RawData.Clear;
  FreeAndNil(RawData);
  inherited;
end;

function TVvarCharactMeasPosition.NewItem : TVvarCharactRawMeasureData;
begin
  Result := TVvarCharactRawMeasureData.Create;
  RawData.Add(Result);
end;

function TVvarCharactMeasPosition.Init: boolean;
var
vSolIdx, vPattIdx : Word;
vItem : TVvarCharactRawMeasureData;
vPatternItem : TCharactItem;
vvTag, vvVar : Byte;
begin
  Result := False;
  if not Assigned(Pattern) then
    Exit;
  RawData.Clear;
  CurrItem := 0;
  vSolIdx := 0;
  while vSolIdx < DbExt.LastIter.MeasSolutions.Count do
  begin
    vPattIdx := 0;
    vvVar := 0;
    vvTag := 0;
    while vPattIdx < Pattern.List.Count do
    begin
      vPatternItem := Pattern.List[vPattIdx];
      if vvTag <> vPatternItem.Step then
      begin
        vvVar := 0;
        vvTag := vPatternItem.Step;
      end;
      vItem := NewItem;
      vItem.Solution := vSolIdx + 1;
      vItem.vTag := vPatternItem.Step;
      vItem.vVar := vvVar;
      vItem.Registers.Assign(DbExt.LastIter.MeasSolutions[vSolIdx].Registers);
      vItem.Registers.Assign(vPatternItem, Pattern.fCharactBitsIdx);
      if (vItem.vTag <> C_INF_Index) then
        vItem.Registers.Registers.INF := DbExt.LastIter.MeasSolutions[vSolIdx].Registers.Registers.INF;
      vItem.Registers.Registers.TEST := 6;
      Inc(vvVar);
      inc(vPattIdx);
    end;
    Inc(vSolIdx);
  end;
  Result := (RawData.Count > 0);
  if Result then
    Chip.Assign(RawData[0].Registers);
end;

procedure TVvarCharactMeasPosition.WriteMeasData(const ADB : TEmbSQLDataBase);
const
cInsertSqlText = 'INSERT INTO VVAR_CHAR (ITER_NUM, SOL_NUM, IZM_NUM, POINTIDX, METHOD_TAG, METHOD_VAR, TEMPR, VOLTAGE, REGMEM, ERR_CODE, VDD, VC)'+
      'VALUES (:ITER_NUM, :SOL_NUM, :IZM_NUM, :POINTIDX, :METHOD_TAG, :METHOD_VAR, :TEMPR, :VOLTAGE, :REGMEM, :ERR_CODE, :VDD, :VC)';
cSelMaxIzmNumSqtText = 'SELECT MAX(IZM_NUM) FROM VVAR_CHAR WHERE ITER_NUM = :ITER_NUM AND SOL_NUM = :SOL_NUM';
var
vQR : TEmbSQLRequest;
vIdx, vIzmNum : Word;
vErrAd : TVChErrorsAdapter;
vMS : TMemoryStream;
vSolNum : byte;
vBoardLogger : IdmLogger;
begin
  vMS := TMemoryStream.Create;
  try
    ADB.TransactionBegin;
    try
      vIdx := 0;
      vSolNum := 0;
      vIzmNum := 1;
      while vIdx < RawData.Count do
      begin
        if ((vIdx = 0)
        or (vSolNum <> RawData[vIdx].Solution)) then
        begin
          vIzmNum := 1;
          vQR.Prepare(ADB.DB, cSelMaxIzmNumSqtText);
          vQR.Bind(1, DbExt.LastIter.Num);
          vQR.Bind(2, RawData[vIdx].Solution);
          if (vQR.PrepareNext = SQLEMB_DONE)
            and (vQR.Step = SQLEMB_ROW) then
            vIzmNum := vQR.FieldInt(0)+1;
          vSolNum := RawData[vIdx].Solution;
          vQR.Close;
        end;
        vMS.Clear;
        RawData[vIdx].Registers.SaveToStream(vMS);
        vMS.Position := 0;
        vQR.Prepare(ADB.DB, cInsertSqlText);
        vErrAd.i := 0;
        vErrAd.s := RawData[vIdx].ErrCode;
        vQR.Bind(1, DbExt.LastIter.Num);
        vQR.Bind(2, RawData[vIdx].Solution);
        vQR.Bind(3, vIzmNum);
        vQR.Bind(4, PointIdx);
        vQR.Bind(5, RawData[vIdx].vTag);
        vQR.Bind(6, RawData[vIdx].vVar);
        vQR.Bind(7, RawData[vIdx].Tempr);
        vQR.Bind(8, RawData[vIdx].Voltage);
        vQR.Bind(9, vMS);
        vQR.Bind(10, vErrAd.i);
        vQR.Bind(11, RawData[vIdx].VDD);
        vQR.Bind(12, RawData[vIdx].VC);
        vQR.Execute;
        Inc(vIzmNum);
        Inc(vIdx);
      end;
      ADB.Commit;
      DbExt.LastIter.MeasPoints[PointIdx].MeasSuccInclude(rpVvarCharactMeas);
      DbExt.LastIter.Write(ADB, False, [foRange]);
    except
      on e : Exception do
      begin
        ADB.Rollback;
        if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
          vBoardLogger.Log(lWarning, Format('Exception %s "%s" on save vvar charact meas  data', [e.ClassName, e.Message]));
        vBoardLogger := nil;
      end;
    end;
  finally
    FreeAndNil(vMS);
  end;
end;
{$ENDREGION}

{$REGION 'VvarFC'}



{ TVFPattern }

constructor TVFPattern.Create;
begin
  fList := TObjectList<TMilandrRev8Registers>.Create;
end;

destructor TVFPattern.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

procedure TVFPattern.ReadPattern(const ADM: IDMAccess; const AConnection: TvdConnection; AID: integer);
const
C_Sel_Patterns = 'SELECT PROC_PATTERNS.DATA_TEXT '+
'FROM PROC_PATTERNS WHERE PROC_PATTERNS.PATTERN_ID = :PATTERN_ID ';
var
vQR : TvdQuery;
vSt : TStringList;
vIni : TMemIniFile;
vIdx : Word;
begin
  fList.Clear;
  ID := AID;
  vQR:= ADM.CreateReadQuery(nil, C_Sel_Patterns, AConnection);
  vSt := TStringList.Create;
  vIni := TMemIniFile.Create('');
  try
    try
      vQR.Prepare;
      vQR.ParamByName('PATTERN_ID').AsInteger := AID;
      vQR.Open;
      if vQR.RecordCount > 0 then
      begin
        vSt.Clear;
        vSt.Text := vQR.FieldByName('DATA_TEXT').AsWideString;
        vIni.SetStrings(vSt);
        vSt.Clear;
        vIni.ReadSections(vST);
        vIdx := 0;
        while vIdx < vST.Count do
        begin
          if vIni.SectionExists(Format('Stp8VvarForceMeasReg%d',[vIdx])) then
          begin
            fList.Add(TMilandrRev8Registers.Create(nil));
            fList.Last.LoadFromIni(vIni, Format('Stp8VvarForceMeasReg%d',[vIdx]));
          end;
          Inc(vIdx);
        end;
      end;
      vQR.Close;
    except
      on e : exception do
        ADM.OnException(vQR, E);
    end;
  finally
    FreeAndNil(vQR);
    FreeAndNil(vIni);
    FreeAndNil(vSt);
  end;
end;

type
TFCItem = class(TObject)
  public
  Registers : TMilandrRev8Registers;
  ErrCode : TVFErrors;
  Voltage,
  Freq,
  DispPpm,
  Tempr,
  VDD, VC : double;
  constructor Create;
  destructor Destroy; override;
end;

TVvarFCPosition = class(TInRangePosition)
 public
  Pattern : TVFPattern;
  CurrItem: Word;
  RawData : TObjectList<TFCItem>;
  constructor Create(const ATestThread : TMilRev8AbstractTestThread); override;
  destructor Destroy; override;
  function Init : boolean; override;
  procedure WriteMeasData(const ADB : TEmbSQLDataBase); override;
end;

{ TFCItem }

constructor TFCItem.Create;
begin
  inherited Create;
  Registers := TMilandrRev8Registers.Create(nil);
end;

destructor TFCItem.Destroy;
begin
  FreeAndNil(Registers);
  inherited Destroy;
end;

constructor TVvarFCPosition.Create(const ATestThread : TMilRev8AbstractTestThread);
begin
  inherited;
  RawData := TObjectList<TFCItem>.Create;
end;

destructor TVvarFCPosition.Destroy;
begin
  RawData.Clear;
  FreeAndNil(RawData);
  inherited;
end;

function TVvarFCPosition.Init: boolean;
var
vFCItem : TFCItem;
vIdx : byte;
begin
  RawData.Clear;
  CurrItem := 0;
  for vIdx := 0 to Pattern.List.Count - 1 do
  begin
    vFCItem := TFCItem.Create;
    vFCItem.Registers.Assign(DBExt.InitRegisters);
    vFCItem.Registers.Assign(Pattern.List[vIdx], TMilRev8InRangeThread(TestThread).fFCRegisters);
    if (vFCItem.Registers.Registers.TEST <> 6)
    or  (vFCItem.Registers.Registers.TEST <> 12)  then
      vFCItem.Registers.Registers.TEST := 6;
    RawData.Add(vFCItem);
  end;
  Result := (RawData.Count > 0);
  if Result then
    Chip.Assign(RawData[0].Registers);
end;

procedure TVvarFCPosition.WriteMeasData(const ADB : TEmbSQLDataBase);
const
cInsertSqlText =
      'INSERT INTO VVAR_FC (ITER_NUM, IZM_NUM, POINTIDX, TEMPR, REGMEM, VOLTAGE, FREQ, FREQDISP, ERR_CODE, VDD, VC) '+
      'VALUES (:ITER_NUM,  :IZM_NUM, :POINTIDX, :TEMPR, :REGMEM, :VOLTAGE, :FREQ, :FREQDISP, :ERR_CODE, :VDD, :VC);';
cSelMaxIzmNumSqtText = 'SELECT MAX(IZM_NUM) FROM VVAR_FC WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
vIdx, vIzmNum : Word;
vErrAd : TVFErrorsAdapter;
vBoardLogger : IdmLogger;
vMS : TMemoryStream;
begin
  vMS := TMemoryStream.Create;
  try
    ADB.TransactionBegin;
    try
      vIdx := 0;
      vIzmNum := 0;
      vQR.Prepare(ADB.DB, cSelMaxIzmNumSqtText);
      vQR.Bind(1, DbExt.LastIter.Num);
      if (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW) then
        vIzmNum := vQR.FieldInt(0)+1;
      vQR.Close;
      while vIdx < RawData.Count do
      begin
        vMS.Clear;
        RawData[vIdx].Registers.SaveToStream(vMS);
        vMS.Position := 0;
        vQR.Prepare(ADB.DB, cInsertSqlText);
        vErrAd.i := 0;
        vErrAd.s := RawData[vIdx].ErrCode;
        vQR.Bind(1, DbExt.LastIter.Num);
        vQR.Bind(2, vIzmNum);
        vQR.Bind(3, PointIdx);
        vQR.Bind(4, RawData[vIdx].Tempr);
        vQR.Bind(5, vMS);
        vQR.Bind(6, RawData[vIdx].Voltage);
        vQR.Bind(7, RawData[vIdx].Freq);
        vQR.Bind(8, RawData[vIdx].DispPpm);
        vQR.Bind(9, vErrAd.i);
        vQR.Bind(10, RawData[vIdx].VDD);
        vQR.Bind(11, RawData[vIdx].VC);
        vQR.Execute;
        Inc(vIdx);
        inc(vIzmNum);
      end;
      ADB.Commit;
      DbExt.LastIter.MeasPoints[PointIdx].MeasSuccInclude(rpVvarFC);
      DbExt.LastIter.Write(ADB, False, [foRange]);
    except
      on e : Exception do
        begin
          ADB.Rollback;
          if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
            vBoardLogger.Log(lWarning, Format('Exception %s "%s" on save vvar force meas  data', [e.ClassName, e.Message]));
          vBoardLogger := nil;
        end;
    end;
  finally
    FreeAndNil(vMS);
  end;
end;

{$ENDREGION}

{$REGION 'FreqZero'}
type
TFreqZeroPosition = class(TInRangePosition)
 public
  ErrCode : TTOErrors;
  VDD, VC,
  Tempr,
  FreqValue, DispPpmValue : double;
  constructor Create(const ATestThread : TMilRev8AbstractTestThread); override;
  function Init : boolean; override;
  procedure WriteMeasData(const ADB : TEmbSQLDataBase); override;
end;

constructor TFreqZeroPosition.Create(const ATestThread : TMilRev8AbstractTestThread);
begin
  inherited;
  ErrCode := [];
end;

function TFreqZeroPosition.Init: boolean;
begin
  Chip.Assign(DbExt.InitRegisters);
  Chip.Registers.LIN := 255;
  Result := True;
end;

procedure TFreqZeroPosition.WriteMeasData(const ADB : TEmbSQLDataBase);
const
cInsertSqlText =
      'INSERT INTO FREQ_TCOFF (ITER_NUM, IZM_NUM, POINTIDX, TEMPR, REGMEM, FREQ, FREQDISP, ERR_CODE, VDD, VC) '+
      'VALUES (:ITER_NUM, :IZM_NUM, :POINTIDX, :TEMPR, :REGMEM, :FREQ, :FREQDISP, :ERR_CODE, :VDD, :VC);';
cSelMaxIzmNumSqtText = 'SELECT MAX(IZM_NUM) FROM FREQ_TCOFF WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
vIzmNum : Word;
vErrAd : TTOErrorsAdapter;
vMS : TMemoryStream;
vBoardLogger : IdmLogger;
begin
  vMS := TMemoryStream.Create;
  try
    ADB.TransactionBegin;
    try
      vIzmNum := 0;
      vQR.Prepare(ADB.DB, cSelMaxIzmNumSqtText);
      vQR.Bind(1, DbExt.LastIter.Num);
      if (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW) then
        vIzmNum := vQR.FieldInt(0)+1;
      vQR.Close;
      vMS.Clear;
      Chip.SaveToStream(vMS);
      vMS.Position := 0;
      vQR.Prepare(ADB.DB, cInsertSqlText);
      vErrAd.i := 0;
      vErrAd.s := ErrCode;
      vQR.Bind(1, DbExt.LastIter.Num);
      vQR.Bind(2, vIzmNum);
      vQR.Bind(3, PointIdx);
      vQR.Bind(4, Tempr);
      vQR.Bind(5, vMS);
      vQR.Bind(6, FreqValue);
      vQR.Bind(7, DispPpmValue);
      vQR.Bind(8, vErrAd.i);
      vQR.Bind(9, VDD);
      vQR.Bind(10, VC);
      vQR.Execute;
      ADB.Commit;
      DbExt.LastIter.MeasPoints[PointIdx].MeasSuccInclude(rpFreqZero);
      DbExt.LastIter.Write(ADB, False, [foRange]);
    except
      on e : Exception do
      begin
        ADB.Rollback;
        if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
          vBoardLogger.Log(lWarning, Format('Exception %s "%s" on save freq TC off meas  data', [e.ClassName, e.Message]));
        vBoardLogger := nil;
      end;
    end;
  finally
    FreeAndNil(vMS);
  end;
end;

{$ENDREGION}

{$REGION 'FreqFinal'}
type
TFreqFinalPosition = class(TInRangePosition)
 public
  ErrCode : TFTErrors;
  VDD, VC,
  Tempr,
  FreqValue, DispPpmValue : double;
  constructor Create(const ATestThread : TMilRev8AbstractTestThread); override;
  procedure WriteMeasData(const ADB : TEmbSQLDataBase); override;
  function Init : boolean; override;
end;

constructor TFreqFinalPosition.Create(const ATestThread : TMilRev8AbstractTestThread);
begin
  inherited;
  ErrCode := [];
end;

function TFreqFinalPosition.Init: boolean;
begin
  Result := True;
end;

procedure TFreqFinalPosition.WriteMeasData(const ADB : TEmbSQLDataBase);
const
cInsertSqlText =
      'INSERT INTO FREQ_FINAL (ITER_NUM, IZM_NUM, POINTIDX, TEMPR, FREQ, FREQDISP, ERR_CODE, VDD, VC) '+
      'VALUES (:ITER_NUM, :IZM_NUM, :POINTIDX, :TEMPR, :FREQ, :FREQDISP, :ERR_CODE, :VDD, :VC);';
cSelMaxIzmNumSqtText = 'SELECT MAX(IZM_NUM) FROM FREQ_FINAL WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
vIzmNum : Word;
vErrAd : TFTErrorsAdapter;
vBoardLogger : IdmLogger;
begin
  ADB.TransactionBegin;
  try
    vIzmNum := 0;
    vQR.Prepare(ADB.DB, cSelMaxIzmNumSqtText);
    vQR.Bind(1, DbExt.LastIter.Num);
    if (vQR.PrepareNext = SQLEMB_DONE)
      and (vQR.Step = SQLEMB_ROW) then
      vIzmNum := vQR.FieldInt(0)+1;
    vQR.Close;
    vQR.Prepare(ADB.DB, cInsertSqlText);
    vErrAd.i := 0;
    vErrAd.s := ErrCode;
    vQR.Bind(1, DbExt.LastIter.Num);
    vQR.Bind(2, vIzmNum);
    vQR.Bind(3, PointIdx);
    vQR.Bind(4, Tempr);
    vQR.Bind(5, FreqValue);
    vQR.Bind(6, DispPpmValue);
    vQR.Bind(7, vErrAd.i);
    vQR.Bind(8, VDD);
    vQR.Bind(9, VC);
    vQR.Execute;
    ADB.Commit;
    DbExt.LastIter.MeasPoints[PointIdx].MeasSuccInclude(rpFreqFinal);
    DbExt.LastIter.Write(ADB, False, [foRange]);
  except
    on e : Exception do
      begin
        ADB.Rollback;
        if Supports(TestThread.Board, IdmLogger, vBoardLogger) then
          vBoardLogger.Log(lWarning, Format('Exception %s "%s" on save freq final meas  data', [e.ClassName, e.Message]));
        vBoardLogger := nil;
      end;
  end;
end;

{$ENDREGION}

const
C_PositionClass : array[TInRangeProcOption] of TProcessedPositionClass = (TVvarCurrMeasPosition,
                                                              TVvarCharactMeasPosition,
                                                              TVvarFCPosition,
                                                              TFreqCurrPosition,
                                                              TFreqZeroPosition,
                                                              TFreqFinalPosition);

function TMilRev8InRangeThread.PositionClass: TProcessedPositionClass;
begin
  if ProcessState = psPrepare then
    Result := inherited PositionClass
  else
    Result := C_PositionClass[fTestSeqence];
end;

function TMilRev8InRangeThread.PreparePosition(
  const APosition: TProcessedPosition): Boolean;

function GetCharactPattern(AID : Integer; var oPattern : TCharactPattern):Boolean;
var
vPoIdx : Word;
begin
  vPoIdx := 0;
  Result := False;
  while (vPoIdx < fPointCharactPatterns.Count) do
  begin
    Result := fPointCharactPatterns[vPoIdx].ID = AID;
    if Result then
      break ;
    Inc(vPoIdx);
  end;
  if Result then
    oPattern := fPointCharactPatterns[vPoIdx]
  else
  try
    oPattern := TCharactPattern.Create;
    fPointCharactPatterns.Add(oPattern);
    oPattern.ReadPattern(fDMAccess, fConnection, AID);
    Result := True;
  except
    on E : Exception do
      fBoardLogger.Log(lException, E.Message);
  end;
end;

function GetVFPattern(AID : Integer; var oPattern : TVFPattern):Boolean;
var
vPoIdx : Word;
begin
  vPoIdx := 0;
  Result := False;
  while (vPoIdx < fVFPatterns.Count) do
  begin
    Result := fVFPatterns[vPoIdx].ID = AID;
    if Result then
      break ;
    Inc(vPoIdx);
  end;
  if Result then
    oPattern := fVFPatterns[vPoIdx]
  else
  try
    oPattern := TVFPattern.Create;
    fVFPatterns.Add(oPattern);
    oPattern.ReadPattern(fDMAccess, fConnection, AID);
    Result := True;
  except
    on E : Exception do
      fBoardLogger.Log(lException, E.Message);
  end;
end;
var
vPointIdx : word;
vPosition : TInRangePosition;
vVvarCharactMeasPosition : TVvarCharactMeasPosition;
vVvarForcePosition : TVvarFCPosition;
begin
  if (ProcessState = psPrepare) then
  begin
    result := (APosition.DbExt.ItersCount > 0)
        and (APosition.DbExt.LastIter.Options.ControlOptions.InRangeOptions[
                APosition.DbExt.LastIter.Options.ControlOptions.MeasureMode].s <> [])
        and APosition.DbExt.LastIter.MeasPoints.Find(fChamberProcess.CurrPointTempr, fChamberProcess.MaxDeltaTempr, vPointIdx)
        and (APosition.DbExt.LastIter.MeasPoints[vPointIdx].MeasSucc.s <> APosition.DbExt.LastIter.Options.ControlOptions.InRangeOptions[
                APosition.DbExt.LastIter.Options.ControlOptions.MeasureMode].s);
    if Result then
    begin
      Result := False;
      if (rpVvarCharactMeas in APosition.DbExt.LastIter.Options.ControlOptions.InRangeOptions[APosition.DbExt.LastIter.Options.ControlOptions.MeasureMode].s)
      and (not (rpVvarCharactMeas in APosition.DbExt.LastIter.MeasPoints[vPointIdx].MeasSucc.s)) then
      begin
        if APosition.DbExt.LastIter.MeasPoints[vPointIdx].PatternExists then
           Result := true;
      end else
        Result := true;
    end;
  end
  else
  begin
    result := (APosition.DbExt.ItersCount > 0)
          and (fTestSeqence in APosition.DbExt.LastIter.Options.ControlOptions.InRangeOptions[
                                  APosition.DbExt.LastIter.Options.ControlOptions.MeasureMode].s)
          and APosition.DbExt.LastIter.MeasPoints.Find(fChamberProcess.CurrPointTempr, fChamberProcess.MaxDeltaTempr, vPointIdx)
          and (not (fTestSeqence in APosition.DbExt.LastIter.MeasPoints[vPointIdx].MeasSucc.s))
          and (APosition is TInRangePosition)
          and (APosition is C_PositionClass[fTestSeqence]);
    if Result then
    begin
      vPosition := APosition as TInRangePosition;
      vPosition.PointIdx := vPointIdx;
      case fTestSeqence of
        rpVvarCharactMeas:
        begin
          Result := (vPosition.DbExt.LastIter.MeasPoints[vPointIdx].PatternExists)
            and (vPosition is TVvarCharactMeasPosition);
          if not Result then
            Exit;
          vVvarCharactMeasPosition := vPosition as TVvarCharactMeasPosition;
          result := GetCharactPattern(APosition.DbExt.LastIter.MeasPoints[vPointIdx].PatternID, vVvarCharactMeasPosition.Pattern)
            and (vVvarCharactMeasPosition.Pattern.List.Count > 0);
        end;
        rpVvarFC:
        begin
          Result := (vPosition is TVvarFCPosition);
          if not Result then
            Exit;
          vVvarForcePosition := vPosition as TVvarFCPosition;
          result := GetVFPattern(APosition.CardProcessOptions.ControlOptions.VVFPatternID, vVvarForcePosition.Pattern)
            and (vVvarForcePosition.Pattern.List.Count > 0);
        end;
        rpFreqFinal:
          Result := APosition.DbExt.LastIter.MeasPoints[vPointIdx].WorkPoint;
      end;
      if Result then
      try
        Result := False;
        result := vPosition.Init;
      except on e : Exception do
        begin
          fBoardLogger.Log(lError, Format(' %s Exception "%s" in Milandr rev.8 in range thread measurer '+
          'during position %d init. process state:%s, chamber target tempr %f°C.',
            [e.ClassName, e.Message,
            vPosition.Position.BoardPos,
            MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence],
            fChamberProcess.CurrPointTempr]));
        end;
      end;
    end;
  end;
end;

function TMilRev8InRangeThread.VvarCurrMeasFunct: Boolean;
var
vPosIdx : Integer;
vSuccCount : word;
vProcessPosition : TVvarCurrMeasPosition;
vCurrTempr, vMaxTemprLag : Double;
vErrors : TVCErrors;
begin
  Result := False;
  vMaxTemprLag := fChamberProcess.MaxDeltaTempr;
  fBoardLogger.Log(lDebug, Format('chamber target tempr %f°C. activate positions for %s',
                  [fChamberProcess.CurrPointTempr,
                  MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]));
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.AfterEnable) then
    Exit;
  if ((not CheckPowerVoltages(vSuccCount))
  or (vSuccCount < 1)) then
    Exit;
  if (vSuccCount < ProcessPositions.Count) then
    DropInProtectPositions;
  if (ProcessPositions.Count < 1) then
    Exit;
  while (ProcessPositions.Count > 0) and (not Terminated) do
  begin
    LogPositonsVoltagesStates;
    {start write}
    if not ProgrammerWriteWorkAndResume(5000) then
      Break;
    if not SleepAndResume(fTimings.Voltage) then
      Break;
    vSuccCount := 0;
    vPosIdx := ProcessPositions.Count-1;

    vCurrTempr := fChamberProcess.CurrTempr;
    while (vPosIdx > -1) and (not Terminated) do
    begin
      vErrors := [];
      try
        vProcessPosition := ProcessPositions[vPosIdx] as TVvarCurrMeasPosition;
        {$IFNDEF PROTECTERRORIGNORE}
        if vProcessPosition.Controller.ProtectState then
          Include(vErrors, vmcProtect);
        {$ENDIF}

        if not SameValue(vProcessPosition.DbExt.LastIter.MeasPoints[vProcessPosition.PointIdx].PointValue,
                    vCurrTempr, vMaxTemprLag) then
          Include(vErrors, mvcTempr);

        fPositionVoltages.VDD := -1E8;
        fPositionVoltages.VC := -1E8;
        fPositionVoltages.Analog := -1E8;
        vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);

        if ((SecondsBetween(Now, fPositionVoltages.VDDTimeStamp) > 5)
        or (SecondsBetween(Now, fPositionVoltages.VCTimeStamp) > 5)
        or (SecondsBetween(Now, fPositionVoltages.AnalogTimeStamp) > 5))   then
          Include(vErrors, vmcVoltOld);

        (*if ((fPositionVoltages.Analog < vProcessPosition.DbExt.LastIter.Options.ErrorLimits.VvarMin)
        or (fPositionVoltages.Analog > vProcessPosition.DbExt.LastIter.Options.ErrorLimits.VvarMax)) then
          Include(vErrors, mvcVoltRange); *)

        if ((not SameValue(fPositionVoltages.VDD,
                      vProcessPosition.CardProcessOptions.VDD,
                      vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance))
         or (not SameValue(fPositionVoltages.VC,
                      vProcessPosition.CardProcessOptions.VC,
                      vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance))) then
          Include(vErrors, vmcVDD_VC);


        vProcessPosition.RawData[vProcessPosition.CurrItem].VvarValue := fPositionVoltages.Analog;
        vProcessPosition.RawData[vProcessPosition.CurrItem].VDD := fPositionVoltages.VDD;
        vProcessPosition.RawData[vProcessPosition.CurrItem].VC := fPositionVoltages.VC;
        vProcessPosition.RawData[vProcessPosition.CurrItem].Tempr := vCurrTempr;
        vProcessPosition.RawData[vProcessPosition.CurrItem].ErrCode := vErrors;
        Inc(vProcessPosition.CurrItem);
        vProcessPosition.Succ := (vProcessPosition.CurrItem = vProcessPosition.RawData.Count);
        if vProcessPosition.Succ then
        begin
          try
            vProcessPosition.SaveMeasData;
            Inc(vSuccCount);
          finally
            ProcessPositions.Delete(vPosIdx);
          end;
        end else
          vProcessPosition.Chip.Assign(vProcessPosition.RawData[vProcessPosition.CurrItem].Registers);
      finally
        Dec(vPosIdx);
      end;
    end;
    if (vSuccCount > 0) then
    begin
      SetActiveProcessedPositions;
      if not SendEnables(3000) then
      begin
        Break;
        Terminate;
      end;
    end;
  end;
  Result := ProcessPositions.Count = 0;
end;

function TMilRev8InRangeThread.VvarCharactMeasFunct: Boolean;
var
vPosIdx : Integer;
vSuccCount : word;
vProcessPosition : TVvarCharactMeasPosition;
vCurrTempr, vMaxTemprLag : Double;
vErrors : TVChErrors;
begin
  Result := False;
  vMaxTemprLag := fChamberProcess.MaxDeltaTempr;
  fBoardLogger.Log(lDebug, Format('activate positions for %s',
                  [MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]));
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.AfterEnable) then
    Exit;
  if ((not CheckPowerVoltages(vSuccCount))
  or (vSuccCount < 1)) then
    Exit;
  if (vSuccCount < ProcessPositions.Count) then
    DropInProtectPositions;
  if (ProcessPositions.Count < 1) then
    Exit;
  while (ProcessPositions.Count > 0) and (not Terminated) do
  begin
    LogPositonsVoltagesStates;
    {start write}
    if not ProgrammerWriteWorkAndResume(5000) then
      Break;
    if not SleepAndResume(fTimings.Voltage) then
      Break;
    vPosIdx := ProcessPositions.Count-1;
    vSuccCount := 0;
    vCurrTempr := fChamberProcess.CurrTempr;
    while (vPosIdx > -1) and (not Terminated) do
    begin
      vErrors := [];
      try
        vProcessPosition := ProcessPositions[vPosIdx] as TVvarCharactMeasPosition;
        {$IFNDEF PROTECTERRORIGNORE}
        if vProcessPosition.Controller.ProtectState then
          Include(vErrors, mveProtect);
        {$ENDIF}

        if not SameValue(vProcessPosition.DbExt.LastIter.MeasPoints[vProcessPosition.PointIdx].PointValue,
                    vCurrTempr, vMaxTemprLag) then
          Include(vErrors, mveTempr);
        fPositionVoltages.VDD := -1E8;
        fPositionVoltages.VC := -1E8;
        fPositionVoltages.Analog := -1E8;
        vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);

        if ((SecondsBetween(Now, fPositionVoltages.VDDTimeStamp) > 5)
        or (SecondsBetween(Now, fPositionVoltages.VCTimeStamp) > 5)
        or (SecondsBetween(Now, fPositionVoltages.AnalogTimeStamp) > 5))   then
          Include(vErrors, mveVoltOld);

        (*if ((fPositionVoltages.Analog < vProcessPosition.DbExt.LastIter.Options.ErrorLimits.VvarMin)
        or (fPositionVoltages.Analog > vProcessPosition.DbExt.LastIter.Options.ErrorLimits.VvarMax)) then
          Include(vErrors, mveVoltRange); *)

        if ((not SameValue(fPositionVoltages.VDD,
                      vProcessPosition.CardProcessOptions.VDD,
                      vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance))
         or (not SameValue(fPositionVoltages.VC,
                      vProcessPosition.CardProcessOptions.VC,
                      vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance))) then
           Include(vErrors, vmeVDD_VC);

        vProcessPosition.RawData[vProcessPosition.CurrItem].Voltage := fPositionVoltages.Analog;
        vProcessPosition.RawData[vProcessPosition.CurrItem].VDD := fPositionVoltages.VDD;
        vProcessPosition.RawData[vProcessPosition.CurrItem].VC := fPositionVoltages.VC;
        vProcessPosition.RawData[vProcessPosition.CurrItem].Tempr := vCurrTempr;
        vProcessPosition.RawData[vProcessPosition.CurrItem].ErrCode := vErrors;
        Inc(vProcessPosition.CurrItem);
        vProcessPosition.Succ := (vProcessPosition.CurrItem = vProcessPosition.RawData.Count);
        if vProcessPosition.Succ then
        begin
          try
            vProcessPosition.SaveMeasData;
            Inc(vSuccCount);
          finally
            ProcessPositions.Delete(vPosIdx);
          end;
        end else
          vProcessPosition.Chip.Assign(vProcessPosition.RawData[vProcessPosition.CurrItem].Registers);
      finally
        Dec(vPosIdx);
      end;
    end;
    if (vSuccCount > 0) then
    begin
      SetActiveProcessedPositions;
      if not SendEnables(3000) then
      begin
        Break;
        Terminate;
      end;
    end;
  end;
  Result := ProcessPositions.Count = 0;
end;

function TMilRev8InRangeThread.VvarFCMeasFunct: Boolean;
var
vPosIdx : Integer;
vSuccCount : Word;
vProcessPosition : TVvarFCPosition;
vErrors : TVFErrors;
vCurrTempr, vMaxTemprLag : Double;
begin
  Result := False;
  vMaxTemprLag := fChamberProcess.MaxDeltaTempr;
  fBoardLogger.Log(lDebug, Format('activate positions for %s',[MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]));
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.AfterEnable) then
    Exit;
  if ((not CheckPowerVoltages(vSuccCount))
  or (vSuccCount < 1)) then
    Exit;
  if (vSuccCount < ProcessPositions.Count) then
    DropInProtectPositions;
  if (ProcessPositions.Count < 1) then
    Exit;

  while (ProcessPositions.Count > 0) and (not Terminated) do
  begin
    LogPositonsVoltagesStates;
    {start write}
    if not ProgrammerWriteWorkAndResume(5000) then
      Break;
    if not SleepAndResume(fTimings.Freq) then
      Break;
    {start freq meas}
    if not FreqStartAndResume then
      Break;
    vSuccCount := 0;
    vCurrTempr := fChamberProcess.CurrTempr;
    vPosIdx := ProcessPositions.Count-1;
    while (vPosIdx > -1) and (not Terminated) do
    begin
      vErrors := [];
      try
        vProcessPosition := ProcessPositions[vPosIdx] as TVvarFCPosition;
        fPositionVoltages.VDD := -1E8;
        fPositionVoltages.VC := -1E8;
        vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);
        (*if (fPositionVoltages.Analog > vProcessPosition.DbExt.LastIter.Options.ErrorLimits.VvarMin)
        and (fPositionVoltages.Analog < vProcessPosition.DbExt.LastIter.Options.ErrorLimits.VvarMax) then
        begin *)
          {$IFNDEF PROTECTERRORIGNORE}
          if vProcessPosition.Controller.ProtectState then
            Include(vErrors, fceProtect);
          {$ENDIF}

          if not SameValue(vProcessPosition.DbExt.LastIter.MeasPoints[vProcessPosition.PointIdx].PointValue,
                      vCurrTempr, vMaxTemprLag) then
            Include(vErrors, fceTempr);


          if ((SecondsBetween(Now, fPositionVoltages.VDDTimeStamp) > 5)
          or (SecondsBetween(Now, fPositionVoltages.VCTimeStamp) > 5))  then
               Include(vErrors, fceVoltOld);

          if ((not SameValue(fPositionVoltages.VDD,
                        vProcessPosition.CardProcessOptions.VDD,
                        vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance))
           or (not SameValue(fPositionVoltages.VC,
                        vProcessPosition.CardProcessOptions.VC,
                        vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance))) then
             Include(vErrors, fceVDD_VC);
          fFreqResult.Freq := 1;
          if ((not vProcessPosition.Freq.FreqResult(fFreqResult))
          or ((SecondsBetween(Now, fFreqResult.TimeStamp) > 5)
            or (fFreqResult.SeriesSuccesCount < fFreqResult.SeriesCount div 2 ))) then
             Include(vErrors, fceMeasError);

          if (AbsPpm(fFreqResult.Freq, vProcessPosition.DbExt.Nominal) > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDeviation) then
            Include(vErrors, fceDevMax);
          if (fFreqResult.DispersionPpm > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDispersion) then
            Include(vErrors, fceDispMax);

          vProcessPosition.RawData[vProcessPosition.CurrItem].VDD := fPositionVoltages.VDD;
          vProcessPosition.RawData[vProcessPosition.CurrItem].VC := fPositionVoltages.VC;
          vProcessPosition.RawData[vProcessPosition.CurrItem].Voltage := fPositionVoltages.Analog;
          vProcessPosition.RawData[vProcessPosition.CurrItem].Tempr := vCurrTempr;
          vProcessPosition.RawData[vProcessPosition.CurrItem].Freq :=  fFreqResult.Freq;
          vProcessPosition.RawData[vProcessPosition.CurrItem].DispPpm := fFreqResult.DispersionPpm;
          vProcessPosition.RawData[vProcessPosition.CurrItem].ErrCode := vErrors;
          vProcessPosition.RawData[vProcessPosition.CurrItem].Registers.Assign(vProcessPosition.Chip);
        //end;
        Inc(vProcessPosition.CurrItem);
        vProcessPosition.Succ := (vProcessPosition.CurrItem = vProcessPosition.RawData.Count);
        if vProcessPosition.Succ then
        begin
          try
            vProcessPosition.SaveMeasData;
            Inc(vSuccCount);
          finally
            ProcessPositions.Delete(vPosIdx);
          end;
        end else
          vProcessPosition.Chip.Assign(vProcessPosition.RawData[vProcessPosition.CurrItem].Registers);
      finally
        Dec(vPosIdx);
      end;
    end;
    if (vSuccCount > 0) then
    begin
      SetActiveProcessedPositions;
      if not SendEnables(3000) then
      begin
        Break;
        Terminate;
      end;
    end;
  end;
  Result := ProcessPositions.Count = 0;
end;

function TMilRev8InRangeThread.FreqCurrMeasFunct: Boolean;
var
vPosIdx : Integer;
vSuccCount : word;
vProcessPosition : TFreqCurrPosition;
vErrors : TFMErrors;
vCurrTempr, vMaxTemprLag : Double;
begin
  Result := False;
  vMaxTemprLag := fChamberProcess.MaxDeltaTempr;
  fBoardLogger.Log(lDebug, Format('activate positions for %s',[MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]));
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.AfterEnable) then
    Exit;
  if ((not CheckPowerVoltages(vSuccCount))
    or (vSuccCount < 1)) then
    Exit;
  if (vSuccCount < ProcessPositions.Count) then
    DropInProtectPositions;
  if (ProcessPositions.Count < 1) then
    Exit;
  while (ProcessPositions.Count > 0) and (not Terminated) do
  begin
    LogPositonsVoltagesStates;
    {start write}
    if not ProgrammerWriteWorkAndResume(5000) then
      Break;
    if not SleepAndResume(fTimings.Freq) then
      Break;
    {start freq meas}
    if not FreqStartAndResume then
      Break;
    vPosIdx := ProcessPositions.Count-1;
    vSuccCount := 0;
    vCurrTempr := fChamberProcess.CurrTempr;
    while (vPosIdx > -1) and (not Terminated) do
    begin
      vErrors := [];
      try
        vProcessPosition := ProcessPositions[vPosIdx] as TFreqCurrPosition;
        {$IFNDEF PROTECTERRORIGNORE}
        if vProcessPosition.Controller.ProtectState then
          Include(vErrors, fmeProtect);
        {$ENDIF}

        if not SameValue(vProcessPosition.DbExt.LastIter.MeasPoints[vProcessPosition.PointIdx].PointValue,
                    vCurrTempr, vMaxTemprLag) then
          Include(vErrors, fmeTempr);

        fPositionVoltages.VDD := -1E8;
        fPositionVoltages.VC := -1E8;
        fPositionVoltages.Analog := -1E8;
        vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);
        if ((SecondsBetween(Now, fPositionVoltages.VDDTimeStamp) > 5)
        or (SecondsBetween(Now, fPositionVoltages.VCTimeStamp) > 5))  then
          Include(vErrors, fmeVoltOld);

        if ((not SameValue(fPositionVoltages.VDD,
                      vProcessPosition.CardProcessOptions.VDD,
                      vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance))
         or (not SameValue(fPositionVoltages.VC,
                      vProcessPosition.CardProcessOptions.VC,
                      vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance))) then
           Include(vErrors, fmeVDD_VC);
        fFreqResult.Freq := 1;
        if ((not vProcessPosition.Freq.FreqResult(fFreqResult))
        or ((SecondsBetween(Now, fFreqResult.TimeStamp) > 5)
          or (fFreqResult.SeriesSuccesCount < fFreqResult.SeriesCount div 2 ))) then
           Include(vErrors, fmeMeasError);

        if (AbsPpm(fFreqResult.Freq, vProcessPosition.DbExt.Nominal) > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDeviation) then
          Include(vErrors, fmeDevMax);
        if (fFreqResult.DispersionPpm > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDispersion) then
          Include(vErrors, fmeDispMax);

        vProcessPosition.RawData[vProcessPosition.CurrItem].VDD := fPositionVoltages.VDD;
        vProcessPosition.RawData[vProcessPosition.CurrItem].VC := fPositionVoltages.VC;
        vProcessPosition.RawData[vProcessPosition.CurrItem].Tempr := vCurrTempr;
        vProcessPosition.RawData[vProcessPosition.CurrItem].Freq :=  fFreqResult.Freq;
        vProcessPosition.RawData[vProcessPosition.CurrItem].DispPpm := fFreqResult.DispersionPpm;
        vProcessPosition.RawData[vProcessPosition.CurrItem].ErrCode := vErrors;
        Inc(vProcessPosition.CurrItem);
        vProcessPosition.Succ := (vProcessPosition.CurrItem = vProcessPosition.RawData.Count);
        if vProcessPosition.Succ then
        begin
          try
            vProcessPosition.SaveMeasData;
            Inc(vSuccCount);
          finally
            ProcessPositions.Delete(vPosIdx);
          end;
        end else
          vProcessPosition.Chip.Assign(vProcessPosition.RawData[vProcessPosition.CurrItem].Registers);
      finally
        Dec(vPosIdx);
      end;
    end;
    if (vSuccCount > 0) then
    begin
      SetActiveProcessedPositions;
      if not SendEnables(3000) then
      begin
        Break;
        Terminate;
      end;
    end;
  end;
  Result := ProcessPositions.Count = 0;
end;

function TMilRev8InRangeThread.FreqZeroMeasFunct: Boolean;
var
vIdx : Integer;
vSuccCount : word;
vProcessPosition : TFreqZeroPosition;
vCurrTempr, vMaxTemprLag : Double;
vErrors : TTOErrors;
begin
  Result := False;
    fBoardLogger.Log(lDebug, Format('activate positions for %s',[MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]));
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.AfterEnable) then
    Exit;
  if ((not CheckPowerVoltages(vSuccCount))
    or (vSuccCount < 1)) then
    Exit;
  if (vSuccCount < ProcessPositions.Count) then
    DropInProtectPositions;
  if (ProcessPositions.Count < 1) then
    Exit;
  {start write}
  if not ProgrammerWriteWorkAndResume(5000) then
    Exit;
  if not SleepAndResume(fTimings.Freq) then
    Exit;
  LogPositonsVoltagesStates;
  {start freq meas}
  if not FreqStartAndResume then
    Exit;
  vCurrTempr := fChamberProcess.CurrTempr;
  vMaxTemprLag := fChamberProcess.MaxDeltaTempr;
  for vIdx := 0 to ProcessPositions.Count - 1 do
  begin
    if Terminated then
      break;
    vErrors := [];
    try
      vProcessPosition := ProcessPositions[vIdx] as TFreqZeroPosition;
      {$IFNDEF PROTECTERRORIGNORE}
      if vProcessPosition.Controller.ProtectState then
        Include(vErrors, toeProtect);
      {$ENDIF}
      if not SameValue(vProcessPosition.DbExt.LastIter.MeasPoints[vProcessPosition.PointIdx].PointValue,
                  vCurrTempr, vMaxTemprLag) then
        Include(vErrors, toeTempr);

      fPositionVoltages.VDD := -1E8;
      fPositionVoltages.VC := -1E8;
      fPositionVoltages.Analog := -1E8;
      vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);
      if ((SecondsBetween(Now, fPositionVoltages.VDDTimeStamp) > 5)
      or (SecondsBetween(Now, fPositionVoltages.VCTimeStamp) > 5))  then
           Include(vErrors, toeVoltOld);

      if ((not SameValue(fPositionVoltages.VDD,
                    vProcessPosition.CardProcessOptions.VDD,
                    vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance))
       or (not SameValue(fPositionVoltages.VC,
                    vProcessPosition.CardProcessOptions.VC,
                    vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance))) then
         Include(vErrors, toeVDD_VC);
      fFreqResult.Freq := 1;
      if ((not vProcessPosition.Freq.FreqResult(fFreqResult))
      or ((SecondsBetween(Now, fFreqResult.TimeStamp) > 5)
        or (fFreqResult.SeriesSuccesCount < fFreqResult.SeriesCount div 2 ))) then
         Include(vErrors, toeMeasError);

      if (AbsPpm(fFreqResult.Freq, vProcessPosition.DbExt.Nominal) > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDeviation) then
        Include(vErrors, toeDevMax);
      if (fFreqResult.DispersionPpm > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDispersion) then
        Include(vErrors, toeDispMax);

      vProcessPosition.VDD := fPositionVoltages.VDD;
      vProcessPosition.VC := fPositionVoltages.VC;
      vProcessPosition.Tempr := vCurrTempr;
      vProcessPosition.FreqValue :=  fFreqResult.Freq;
      vProcessPosition.DispPpmValue := fFreqResult.DispersionPpm;
      vProcessPosition.ErrCode := vErrors;
      try
        vProcessPosition.SaveMeasData;
      except
        Continue;
      end;
    except
      Continue;
    end;
  end;
  vSuccCount := 0;
  for vIdx := 0 to ProcessPositions.Count - 1 do
  begin
    if Terminated then
      break;
    vProcessPosition := ProcessPositions[vIdx] as TFreqZeroPosition;
    vProcessPosition.Position.Active := (rpFreqFinal in vProcessPosition.DbExt.LastIter.Options.ControlOptions.InRangeOptions[
    vProcessPosition.DbExt.LastIter.Options.ControlOptions.MeasureMode].s);
    if vProcessPosition.Position.Active then
      inc(vSuccCount);
  end;
  if vSuccCount > 0 then
  begin
    if not ProgrammerZState(5000) then
      exit;
    if not SendDisablesActive(5000) then
      Exit;
    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;
    if not SendEnables(3000) then
      Exit;
  end else
  begin
    for vIdx := 0 to ProcessPositions.Count - 1 do
    begin
      if Terminated then
        break;
      vProcessPosition := ProcessPositions[vIdx] as TFreqZeroPosition;
      vProcessPosition.Position.Active := true;
    end;
  end;
  result := true;
end;

function TMilRev8InRangeThread.FreqFinalMeasFunct: Boolean;
var
vPosIdx : Integer;
vSuccCount, vTurnOffOnCount : word;
vProcessPosition : TFreqFinalPosition;
vCurrTempr, vMaxTemprLag : Double;
vErrors : TFTErrors;
begin
  Result := False;
  vTurnOffOnCount := 0;
  vPosIdx := 0;
  while vPosIdx < ProcessPositions.Count do
  begin
    if Terminated then
      break;
    vProcessPosition := ProcessPositions[vPosIdx] as TFreqFinalPosition;
    vProcessPosition.Position.Active := vProcessPosition.CardProcessOptions.ControlOptions.PowerTurnOffOnInFinalTest;
    if vProcessPosition.Position.Active then
      Inc(vTurnOffOnCount);
    inc(vPosIdx);
  end;
  if Terminated then
    exit;
  if vTurnOffOnCount > 1 then
  begin
    if not ProgrammerZState(5000) then
      exit;
    if not SendDisablesActive(5000) then
      Exit;
    if not SleepAndResume(fTimings.AfterEnable) then
      Exit;
    if Terminated then
      exit;
  end;
  vPosIdx := 0;
  while vPosIdx < ProcessPositions.Count do
  begin
    if Terminated then
      break;
    vProcessPosition := ProcessPositions[vPosIdx] as TFreqFinalPosition;
    vProcessPosition.Position.Active := True;
    inc(vPosIdx);
  end;
  if not SendEnables(3000) then
    Exit;
  if not SleepAndResume(fTimings.AfterEnable) then
    Exit;

  if ((not CheckPowerVoltages(vSuccCount))
    or (vSuccCount < 1)) then
    Exit;
  if (vSuccCount < ProcessPositions.Count) then
    DropInProtectPositions;
  if (ProcessPositions.Count < 1) then
    Exit;
  if not SleepAndResume(3000) then
    Exit;
  {start freq meas}
  if not FreqStartAndResume then
    Exit;
  LogPositonsVoltagesStates;
  vPosIdx := ProcessPositions.Count-1;
  vCurrTempr := fChamberProcess.CurrTempr;
  vMaxTemprLag := fChamberProcess.MaxDeltaTempr;
  while (vPosIdx > -1)  and (not Terminated) do
  begin
    vErrors := [];
    try
      vProcessPosition := ProcessPositions[vPosIdx] as TFreqFinalPosition;
      {$IFNDEF PROTECTERRORIGNORE}
      if vProcessPosition.Controller.ProtectState then
        Include(vErrors, fteProtect);
      {$ENDIF}

      if not SameValue(vProcessPosition.DbExt.LastIter.MeasPoints[vProcessPosition.PointIdx].PointValue,
                  vCurrTempr, vMaxTemprLag) then
        Include(vErrors, fteTempr);

      fPositionVoltages.VDD := -1E8;
      fPositionVoltages.VC := -1E8;
      fPositionVoltages.Analog := -1E8;
      vProcessPosition.Voltages.GetPositionVoltage(fPositionVoltages);
      if ((SecondsBetween(Now, fPositionVoltages.VDDTimeStamp) > 5)
      or (SecondsBetween(Now, fPositionVoltages.VCTimeStamp) > 5))  then
           Include(vErrors, fteVoltOld);

      if ((not SameValue(fPositionVoltages.VDD,
                    vProcessPosition.CardProcessOptions.VDD,
                    vProcessPosition.CardProcessOptions.ErrorLimits.VDDTolerance))
       or (not SameValue(fPositionVoltages.VC,
                    vProcessPosition.CardProcessOptions.VC,
                    vProcessPosition.CardProcessOptions.ErrorLimits.VCTolerance))) then
         Include(vErrors, fteVDD_VC);
      fFreqResult.Freq := 1;
      if ((not vProcessPosition.Freq.FreqResult(fFreqResult))
      or ((SecondsBetween(Now, fFreqResult.TimeStamp) > 5)
        or (fFreqResult.SeriesSuccesCount < fFreqResult.SeriesCount div 2 ))) then
         Include(vErrors, fteMeasError);

      if (AbsPpm(fFreqResult.Freq, vProcessPosition.DbExt.Nominal) > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDeviation) then
        Include(vErrors, fteDevMax);
      if (fFreqResult.DispersionPpm > vProcessPosition.CardProcessOptions.ErrorLimits.ppmMaxDispersion) then
        Include(vErrors, fteDispMax);

      vProcessPosition.VDD := fPositionVoltages.VDD;
      vProcessPosition.VC := fPositionVoltages.VC;
      vProcessPosition.Tempr := vCurrTempr;
      vProcessPosition.FreqValue :=  fFreqResult.Freq;
      vProcessPosition.DispPpmValue := fFreqResult.DispersionPpm;
      vProcessPosition.ErrCode := vErrors;
      try
        vProcessPosition.SaveMeasData;
      finally
        ProcessPositions.Delete(vPosIdx);
      end;
    finally
      Dec(vPosIdx);
    end;
  end;
  result := ProcessPositions.Count = 0;
end;

procedure TMilRev8InRangeThread.StartMessage;
var
vMainProcess : IMainProcess;
begin
  if Supports(Self.MainProc, IMainProcess, vMainProcess) then
    vMainProcess.DoInfoMessage(Format('Плата %d старт измерителя "%s"',[fAbstractBoard.SerialNum,
              MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]),
              0);
  vMainProcess := nil;
end;

procedure TMilRev8InRangeThread.FinishMessage;
var
vMainProcess : IMainProcess;
begin
  if Supports(Self.MainProc, IMainProcess, vMainProcess) then
  begin
    if fExecResult < 1 then
      vMainProcess.DoInfoMessage(Format('Плата %d "%s" завершен успешно',[fAbstractBoard.SerialNum,
              MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]),
              0)
    else
      vMainProcess.DoInfoMessage(Format('Плата %d "%s" завершен c ошибкой',[fAbstractBoard.SerialNum,
              MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]),
              1);
  end;
  vMainProcess := nil;
end;


type
TExecFunciton = function : Boolean of object;
TInternalSequence = (isFreqFinal, isVvarCharactMeas, isVvarFC, isVvarCurrMeas,
               isFreqCurr, isFreqZero);

procedure TMilRev8InRangeThread.StartMain;
var
vExecFunct : array[TInternalSequence] of TExecFunciton;
vTestSeqence : TInternalSequence;
begin
  fPointCharactPatterns := TObjectList<TCharactPattern>.Create;
  fVFPatterns := TObjectList<TVFPattern>.Create;
  try
    vExecFunct[isVvarCharactMeas] := VvarCharactMeasFunct;
    vExecFunct[isVvarFC] :=  VvarFCMeasFunct;
    vExecFunct[isVvarCurrMeas] := VvarCurrMeasFunct;
    vExecFunct[isFreqCurr] :=  FreqCurrMeasFunct;
    vExecFunct[isFreqZero] :=  FreqZeroMeasFunct;
    vExecFunct[isFreqFinal] := FreqFinalMeasFunct;
    for vTestSeqence := Low(vTestSeqence) to High(vTestSeqence) do
    begin
      if Terminated then
        break;
      case vTestSeqence of
        isVvarCharactMeas:
          fTestSeqence := rpVvarCharactMeas;
        isVvarFC:
          fTestSeqence := rpVvarFC;
        isVvarCurrMeas:
          fTestSeqence := rpVvarCurrMeas;
        isFreqCurr:
          fTestSeqence := rpFreqCurr;
        isFreqFinal:
          fTestSeqence := rpFreqFinal;
        isFreqZero:
          fTestSeqence := rpFreqZero;
      end;
      fBoardLogger.Log(lDebug, Format('chamber target tempr %f°C. prepare positions for "%s"',
                          [fChamberProcess.CurrPointTempr,
                          MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]));
      if (fTestSeqence = rpVvarFC) then
        ReadControlledBitsIndex(TMilandrRev8Registers, 'Stp8VvarForceMeasRegisters', false, fFCRegisters);
      FillProcessPositions;
      SetActiveProcessedPositions;
      if (ProcessPositions.Count > 0) then
      begin
        fExecResult := 1;
        if Terminated then
          break;
        Synchronize(StartMessage);
        if SendPositionsVoltagesAndResume(5000) then
        begin
          try
            if vExecFunct[vTestSeqence] then
            begin
              fExecResult := 0;
              Synchronize(FinishMessage);
            end else if Terminated then
              Break
            else
              Synchronize(FinishMessage);
          except on e : Exception do
            fBoardLogger.Log(lError, Format(' %s Exception "%s" in Milandr rev.8 in range measurer during "%s"'+
            'Proc wait state:%s',
            [e.ClassName, e.Message,
            MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence],
            C_ProcWaitStateStr[ProcessState]]));
          end;
        end else if Terminated then
              Break;
      end else
      begin
        fExecResult := 0;
        fBoardLogger.Log(lDebug, Format('no positions for "%s"',
                        [MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence]]));
      end;
      if Terminated then
        break;
    end;
  finally
    SetLength(fFCRegisters, 0);
    FreeAndNil(fPointCharactPatterns);
    FreeAndNil(fVFPatterns);
    fBoardLogger.Log(lEvent, Format('Milandr rev.8 in range measurer finished in state:"%s". Exec result:%d',
    [MilandrRev8Stp8dboExtention.C_InRangeProcOptionTxt[fTestSeqence], fExecResult]));
  end;
end;


end.
