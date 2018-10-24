unit MilandrRev8Stp8dboExtention;

interface
  uses
  System.Classes, System.UITypes, System.Generics.Collections,
  System.SysUtils, System.IniFiles,
  Stp8PositionDBExtention, MilandrRev8IMS, MilandrRev8.DboBase,
  Stp8ProcessDBOptionsInterface, VdpDMAccess;
   type
   TStp8milProcessDBOptions = class(TmilProcessDBOptions,
                             IInterface,
                             IStp8ProcessDBOptions)
   public
    CardID : Integer;
    {IInterface}
     function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
     function _AddRef: Integer; stdcall;
     function _Release: Integer; stdcall;
     {IStp8ProcessDBOptions}
     function IStp8ProcessDBOptions.Load = Stp8_Load;
     function IStp8ProcessDBOptions.Save = Stp8_Save;
     function Stp8_Load(const ADMAccess : IDMAccess; const AConnection : TvdConnection; const ACardID : Integer; const ATextOptions : TStrings = nil):boolean;stdcall;
     function Stp8_Save(const ADMAccess : IDMAccess; const AConnection : TvdConnection; const ACardID : Integer):boolean;stdcall;
   end;
   TInProgQueueState = record
       pqState : Boolean;
       pqID : Integer;
       pqResult : Byte;
     end;

   TMilStepsResult = (msrWRTestOk, msrWRTestFail,
                    msrFreqTestOk, msrFreqTestFail,
                    msrInfTrimOk, msrInfTrimFail,
                    msrCDACTrimOk, msrCDACTrimFail,
                    mrsFinalTestOk, mrsFinalTestFail);
   TMilStepsResults = set of TMilStepsResult;

   TStp8milDbExtention = class(TmilDbExtention)
   private
     fLastIter : TMilIteration<TSolution, TMeasure<TSolution>>;
     procedure SetNominal(const Value: double);
     function GetStepsResults: TMilStepsResults;
     procedure SetStepsResults(const Value: TMilStepsResults);
     procedure SetFatalError(Value : boolean);
     function DeletePosition(const AConnection : TvdConnection; const ADMAccess: IDMAccess): boolean;
    protected
     procedure AfterCreate; override;
     procedure BeforeDestroy; override;
    public
     InProgQueueState : TInProgQueueState;
     function ReadPosData(const ADB : TEmbSQLDataBase):Boolean; override;
     function NewIterRequest: boolean; override;
     function RecostructionRequest: boolean; override;
     property LastIter : TMilIteration<TSolution, TMeasure<TSolution>> read fLastIter;
     property Nominal : double read fNominal write SetNominal;
     property StepsResults : TMilStepsResults read GetStepsResults write SetStepsResults;
     procedure StepResultInclude(Value : TMilStepsResult);
     procedure StepResultExclude(Value : TMilStepsResult);
     property FatalError : boolean read fFatalError write SetFatalError;
   end;

   TLegendInfo=(liEmpty, liRCMask, liCreated, liPrepare, liInMeasure, liMeasComplete, liCompensError, liReadyForProg,
    liProgError, liFinalTest, liFinaTestOK, liFinalTestErr, liFatalErr);


   TMilPositionDBView = class(TPositionDBView)
    private
      fLegendState : TLegendInfo;
    protected
     class function PositionDBExtentionClass:TPositionDBExtentionClass; override;
    public
     function InProgMode : Boolean;override;
     function LastIterSucc : Boolean;override;
     class procedure ListLegendFiller(const AList : TList<TLegendItem>); override;
     procedure UpdateLegendValues(SelectedCardId : Integer;
              ACardSelected, ByCardMask : Boolean; const ACheckedList : TList<Boolean>); override;
   end;


  TLegendOptions = set of TLegendInfo;


const
C_ProcessMode_Meas = 'Измерение';
C_ProcessMode_EpromTest = 'Финальный прогон';

{C_ProcessMode_Meas  = 'Triming';
C_ProcessMode_EpromTest  = 'Final test'; }

C_MeasureModeTxt : array [TMeasureMode] of string = (C_ProcessMode_Meas,C_ProcessMode_EpromTest);

C_StartTestOptionsMethodsTxt : array [TStartTestProcOption] of string =('WR Test', 'Freq Test');
C_PreparePointOptionsMethodsTxt : array [TPreparePointProcOption] of string = ({'Inf Trim', }
                                                'CDAC Trim'{,
                                                'Inf Trim'});
C_InRangeProcOptionTxt : array [TInRangeProcOption] of string = ('Vvar Curr',
                                        'Vvar Charact',
                                        'Vvar FC',
                                        'Freq Curr',
                                        'Freq TC OFF',
                                        'Freq Final');

C_AfterCompensProcOptionTxt : array [TAfterCompensProcOption] of string = ('Calc','CDAC','Prog');
C_CalcMethodTxt : array [TCalcMethod] of string = ('Абсолютный','25°С');//,'Peak-To-Peak');
C_MeasureModeMethods : array[TMeasureMode] of TInRangeProcOptionSet = ([rpVvarCurrMeas, rpVvarCharactMeas, rpVvarFC,
               rpFreqCurr, rpFreqZero],[rpFreqFinal]);

C_SelCurrPointPattern = 'SELECT PROC_PATTERNS.PATTERN_ID '+
'FROM PROC_PATTERNS '+
'INNER JOIN PFPOINTS_PATTERN_LINKS ON (PROC_PATTERNS.PATTERN_ID = PFPOINTS_PATTERN_LINKS.PATTERN_ID) '+
'INNER JOIN PFPOINTS ON (PFPOINTS_PATTERN_LINKS.PFPOINT_ID = PFPOINTS.POINT_ID) '+
'WHERE  ((PFPOINTS.POINT_ID = :POINT_ID)  AND  (PROC_PATTERNS.PATTERN_TYPE = :PATTERN_TYPE) '+
'AND (PROC_PATTERNS.CHIP_GUID = :CHIP_GUID)) ORDER BY PFPOINTS.P_INDEX';

const

C_LegendCardMask = 'Маска по МК';
C_LegendPositionCreated =  'Созданные';
C_LegendPositionInPrepare = 'Подготовка';
C_LegendPositionInMeasure = 'Измерения';
C_LegendPositionMeasureFinished = 'Измерения завершены';
C_LegendCompesationNotAvailable = 'Не подлежит компенсации';
C_LegendPositionInProgramming = 'На программирование';
C_LegendPositionProgrammed = 'Програмир. успешно';
C_LegendPositionprogrammingError = 'Програмир. с ошибкой';
C_LegendPositionFinalTest = 'Финальный прогон';
C_LegendPositionSucceded = 'Готовое изделие';
C_LegendPositionFinalTestError = 'Прогон неуспешен';
C_LegendPositionFatalError = 'Фатальная ошибка';


{C_LegendCardMask = 'Batch mask';
C_LegendPositionCreated =  'Created';
C_LegendPositionInPrepare = 'Preparation';
C_LegendPositionInMeasure = 'Measurements';
C_LegendPositionMeasureFinished = 'Measurements completed';
C_LegendCompesationNotAvailable = 'Trimming impossible';
C_LegendPositionInProgramming = 'On programming';
C_LegendPositionProgrammed = 'Programming success';
C_LegendPositionprogrammingError = 'Programming error';
C_LegendPositionFinalTest = 'Final test';
C_LegendPositionSucceded = 'Completed';
C_LegendPositionFinalTestError = 'Final test error';
C_LegendPositionFatalError = 'Fatal error';  }

C_LegendInfoText : array [TLegendInfo] of ShortString=(C_LegendEmpty, C_LegendCardMask,
 C_LegendPositionCreated,C_LegendPositionInPrepare,C_LegendPositionInMeasure,
 C_LegendPositionMeasureFinished, C_LegendCompesationNotAvailable,
 C_LegendPositionInProgramming, {C_LegendPositionProgrammed,} C_LegendPositionprogrammingError,
 C_LegendPositionFinalTest,C_LegendPositionSucceded,
 C_LegendPositionFinalTestError, C_LegendPositionFatalError);

implementation
uses
Data.DB,
Stp8ProcessRCObj,
Stp8ProcessDBOptions,
StpProcessTypes,
ChipAbstractInterface,
PositionInterface,
PositionListInterface,
ExtentionsListInterface,
ChipAbstract,
MilandrRev8.Consts;

{ TmilProcessDBOptions }

const
C_ResFileExt = 'Stp8Rez';

C_SelCardCurrPattern = 'SELECT CARD_PATTERN_LINKS.PATTERN_ID '+
'FROM RCARD  INNER JOIN CARD_PATTERN_LINKS ON (RCARD.ID = CARD_PATTERN_LINKS.CARD_ID) '+
'INNER JOIN PROC_PATTERNS ON (CARD_PATTERN_LINKS.PATTERN_ID = PROC_PATTERNS.PATTERN_ID) '+
'WHERE ((RCARD.ID = :RCARD_ID) AND (PROC_PATTERNS.PATTERN_TYPE = :PATTERN_TYPE)) ORDER BY PROC_PATTERNS.PATTERN_ID';


C_SelCardPoints = 'SELECT PFPOINTS.POINT_ID, '+
'PFPOINTS.POINTVALUE, PFPOINTS.INI_OPTIONS FROM RCARD ' +
'INNER JOIN PFPOINTS ON (RCARD.WORKRANGE = PFPOINTS.RANGE_ID) '+
'WHERE (RCARD.ID = :RCARD_ID) ORDER BY PFPOINTS.P_INDEX';

type
TInstallResource = record
  Connection : TvdConnection;
  DMAccess : IDMAccess;
  Card : Tstp8RouteCard;
  Board:Word;
  PatternLoader : IPatternLoader;
  InsSP : TvdStoredProc;
  CardOptions : TmilProcessDBOptions;
  IniSect : TStringList;
  StartOptionsIni : TMemIniFile;
  FormatSettings: TFormatSettings;
end;
pInstallResource = ^TInstallResource;

procedure DisposeInstallResource(var AResource : Pointer); stdcall;
var
vResource : pInstallResource;
begin
  vResource := pInstallResource(AResource);
  if Assigned(vResource.IniSect) then
    FreeAndNil(vResource.IniSect);
  if Assigned(vResource.StartOptionsIni) then
    FreeAndNil(vResource.StartOptionsIni);
  if Assigned(vResource.InsSP) then
  begin
    vResource.InsSP.Close;
    FreeAndNil(vResource.InsSP);
  end;
  vResource.CardOptions := nil;
  vResource.PatternLoader := nil;
  vResource.DMAccess := nil;
  vResource.Connection := nil;
  vResource.Card := nil;
  Dispose(vResource);
end; exports DisposeInstallResource;

function CreateInstallResource(const AMainProc : TComponent;
                      const AConnection : TvdConnection;
                      const ACard : Tstp8RouteCard; ABoard:Word):pointer; stdcall;
var
vResource : pInstallResource;
begin
  New(vResource);
  vResource.InsSP := nil;
  vResource.StartOptionsIni := nil;
  vResource.IniSect := nil;
  vResource.Connection := AConnection;
  vResource.Card := ACard;
  vResource.Board := ABoard;
  try
    if ((not Supports(AMainProc, IDMAccess, vResource.DMAccess))
    or (not Supports(AMainProc, IPatternLoader, vResource.PatternLoader))) then
    begin
      raise Exception.Create('Exeption on create positions install resource. Could not access to data module or pattern loader');
    end;
    vResource.InsSP := vResource.DMAccess.CreateWriteStoredProc(nil, 'RPOS_INSERT', AConnection);
    vResource.StartOptionsIni := TMemIniFile.Create('');
    vResource.IniSect := TStringList.Create;
    vResource.FormatSettings := TFormatSettings.Create(1033);
    if (not assigned(vResource.Card)
    or (not Assigned(vResource.Card.ProcOptions))
    or (not (vResource.Card.ProcOptions is TmilProcessDBOptions))) then
    begin
      raise Exception.Create('Exeption on create positions install resource. Could not access to card options or selected card is nil');
    end;
    vResource.CardOptions := TmilProcessDBOptions(ACard.ProcOptions);
    if not vResource.PatternLoader.Load(AConnection, vResource.CardOptions.ControlOptions.StartRegistersPatternID, vResource.StartOptionsIni) then
    begin
      raise Exception.Create('Exeption on create positions install resource. Error on base registers pattern load');
    end;
    vResource.StartOptionsIni.ReadSections(vResource.IniSect);
    if (vResource.IniSect.Count < 1)
    or (not vResource.StartOptionsIni.SectionExists('stp8InitRegisters')) then
    begin
      raise Exception.Create('Exeption on create positions install resource. stp8InitRegisters of MAS6279D8 chip ini-file is empty');
    end;
    vResource.InsSP.Prepare;
    Result := vResource;
  except
    on e : Exception do
    begin
      DisposeInstallResource(Pointer(vResource));
      raise e;
    end;
  end;
end; exports CreateInstallResource;

function InstallPosition(const AResource : pointer; ABoardPos : Byte; const ASerial : string):TInstallAnswer; stdcall;
var
vResource : pInstallResource;
vCard_Fail, vSerialNumExists, vPosExists : boolean;
vDbExt : TStp8milDbExtention;
vDB: TEmbSQLDataBase;
vLogItem : TPosLogItem;
vNewID : Integer;
vNewPosData : TNewPosData;
function InternalInstall : Boolean;
begin
  vResource.InsSP.ParamByName('RNUM').AsString := ASerial;
  vResource.InsSP.ParamByName('RCARD').AsInteger := vResource.Card.ID;
  vResource.InsSP.ParamByName('BOARD').AsInteger := vResource.Board;
  vResource.InsSP.ParamByName('BOARDPOS').AsSmallInt := ABoardPos;
  try
    vResource.InsSP.Open;
    vNewID := vResource.InsSP.FieldByName('ID').AsInteger;
    vSerialNumExists := (vResource.InsSP.FieldByName('NUM_EXISTS').AsInteger > 0);
    vPosExists := (vResource.InsSP.FieldByName('POS_EXISTS').AsInteger > 0);
    vCard_Fail := (vResource.InsSP.FieldByName('RCARD_FAIL').AsInteger > 0);
    vResource.DMAccess.Commit(vResource.InsSP);
    result := true;
  except
    on e : Exception do
    begin
      vResource.DMAccess.OnException(vResource.InsSP, E);
      result := false;
    end;
  end;
end;
begin
  vResource := pInstallResource(AResource);
  try
    if not InternalInstall then
    begin
      Result.Errors := [ieException];
      exit;
    end;
    Result.Errors := [];
    if vCard_Fail then
      include(result.Errors, ieCardClosed);
    if vSerialNumExists then
      include(result.Errors, ieNumExists);
    if vPosExists then
      include(result.Errors, iePosExists);
    if Result.Errors = [] then
    begin
      Result.Errors := [iePosFile];
      Result.DbView := TMilPositionDBView.Create;
      vDbExt := TStp8milDbExtention(Result.DbView.DbExtention);
      vDbExt.fBaseID := vNewID;
      vDbExt.fRCardID :=  vResource.Card.ID;
      vDbExt.fSerialNum := ASerial;
      vDbExt.fBoard_SN := vResource.Board;
      vDbExt.fBoard_Pos := ABoardPos;
      vDbExt.Nominal := vResource.Card.Nominal;
      vDbExt.fChipGUID := MilandrRev8.Consts.C_GUID;
      vDbExt.InitRegisters.LoadFromIni(vResource.StartOptionsIni, 'stp8InitRegisters');
      vDbExt.StartOptions.ReadFromCardOptions(vResource.CardOptions);
      vDB := vDbExt.CreateConnection(vResource.DMAccess.DBHomePath+
                                IntToStr(vDbExt.RCardID)+
                                '\'+vDbExt.SerialNum+'.'+C_ResFileExt);
      try
        vLogItem.DT := Now;
        vLogItem.Step := 'Позиция создана';
        vLogItem.Result := 'OK';
        vLogItem.Comment := Format('ID: %d',[vNewID]);
        vLogItem.Save(vDB);
        vDbExt.StartOptions.Write(vResource.StartOptionsIni, vResource.FormatSettings);
        vNewPosData.PositonBaseID := vNewID;
        vNewPosData.Board_SN := vResource.Board;
        vNewPosData.Board_Pos := ABoardPos;
        vNewPosData.Nominal := vResource.Card.Nominal;
        vNewPosData.ChipGUID := MilandrRev8.Consts.C_GUID;
        vNewPosData.SerialNum := ASerial;
        vDbExt.SetViewerPlugin(vResource.StartOptionsIni, 'MilandrRev8Stp8rv.bpl');
        if vDbExt.CreatePosData(vDB,
                      vResource.StartOptionsIni,
                      vNewPosData,
                      vResource.FormatSettings)then
          Result.Errors := [];
      finally

        if vDB.TransactionActive then
          vDB.RollBack;
        vDB.DBClose;
        FreeAndNil(vDB);
        if (Result.Errors <> []) then
        try
          vDbExt.DeletePosition(vResource.Connection, vResource.DMAccess);
          if FileExists(vDbExt.MeasFileName) then
            DeleteFile(vDbExt.MeasFileName);
        finally
          FreeAndNil(Result.DbView);
        end;
      end;
    end;
  except
    Result.Errors := [ieException];
    if Assigned(Result.DbView) then
      FreeAndNil(Result.DbView);
  end;
end; exports InstallPosition;


function Stp8ProcessCardDBOptions:TStp8ProcessDBOptionsClass; stdcall;
begin
  Result := TStp8milProcessDBOptions;
end; exports Stp8ProcessCardDBOptions;

type
TSelectionResource = record
 DMAccess : IDMAccess;
 Connection : TvdConnection;
 SelectQR,
 ProgCheckQR : TvdQuery;
 PosData : TMemIniFile;
 FormatSettings: TFormatSettings;
end;
pSelectionResource = ^TSelectionResource;

function CreateSelectionResource(const ADMAccess : IDMAccess; const AConnection : TvdConnection):Pointer; stdcall;
const
C_SelText = 'SELECT RPOS.RNUM, RPOS.BOARD, '+
    'RPOS.BOARDPOS, RPOS.STATUS, RPOS.RCARD, RCARD.CHIP_GUID '+
    'FROM RPOS INNER JOIN RCARD ON (RPOS.RCARD = RCARD.ID) '+
    'WHERE (RPOS.ID = :ID)';
C_SelInProgText = 'SELECT PROG_QUE.Q_ID, PROG_QUE.POSITION_ID, '+
    'PROG_QUE.RESULT FROM PROG_QUE WHERE (POSITION_ID = :POSITION_ID)';
var
vResource : pSelectionResource;
begin
  New(vResource);
  result := vResource;
  vResource.DMAccess := ADMAccess;
  vResource.Connection := AConnection;
  vResource.PosData := TMemIniFile.Create('');
  vResource.FormatSettings := TFormatSettings.Create(1033);
  vResource.SelectQR := vResource.DMAccess.CreateQuery(nil, vResource.Connection);
  vResource.SelectQR.SQL.Text := C_SelText;
  vResource.ProgCheckQR := vResource.DMAccess.CreateQuery(nil, vResource.Connection);
  vResource.ProgCheckQR.SQL.Text := C_SelInProgText;
  vResource.SelectQR.Prepare;
  vResource.ProgCheckQR.Prepare;
end; exports CreateSelectionResource;

function PositionViewSelectionExec(AResource : Pointer; APositionID : Integer; var oDbView :TPositionDBView):Boolean; stdcall;
var
vResource : pSelectionResource;
vDB : TEmbSQLDataBase;
vDbView : TMilPositionDBView;
vDbExt : TStp8milDbExtention;
begin
  oDbView := nil;
  vResource := pSelectionResource(AResource);
  vResource.PosData.Clear;
  try
    vResource.SelectQR.ParamByName('id').AsInteger := APositionID;
    vResource.SelectQR.Open;
    try
      Result := (vResource.SelectQR.RecordCount > 0);
      if result then
      begin
        vDbView := TMilPositionDBView.Create;
        vDbExt := vDbView.DbExtention as TStp8milDbExtention;
        vDbExt.fBaseID := APositionID;
        vDbExt.fRCardID :=  vResource.SelectQR.FieldByName('rcard').AsInteger;
        vDbExt.fSerialNum := vResource.SelectQR.FieldByName('rnum').AsString;
        vDbExt.fBoard_SN := vResource.SelectQR.FieldByName('board').AsInteger;
        vDbExt.fBoard_Pos := Byte(vResource.SelectQR.FieldByName('boardpos').AsInteger);
        vDbExt.fBoardState := Byte(vResource.SelectQR.FieldByName('status').AsInteger);
        vDB := vDbExt.CreateConnection(vResource.DMAccess.DBHomePath+
                                      IntToStr(vDbView.DbExtention.RCardID)+
                                      '\'+vDbView.DbExtention.SerialNum+'.'+C_ResFileExt);
        try
          result :=  vDbView.DbExtention.ReadPosData(vDB)
          and vDbView.DbExtention.ReadPosData(vDB, vResource.PosData)
          and vDbView.DbExtention.ReadPosData(vResource.PosData, vResource.FormatSettings)
          and IsEqualGUID(vDbView.DbExtention.ChipGUID, MilandrRev8.Consts.C_GUID);
          vDbExt.fSerialNum := vResource.SelectQR.FieldByName('rnum').AsString;
          vDbExt.fBoard_SN := vResource.SelectQR.FieldByName('board').AsInteger;
          vDbExt.fBoard_Pos := Byte(vResource.SelectQR.FieldByName('boardpos').AsInteger);
          vDbExt.fBoardState := Byte(vResource.SelectQR.FieldByName('status').AsInteger);
        finally
          if vDB.TransactionActive then
            vDB.RollBack;
          vDB.DBClose;
          FreeAndNil(vDB);
          if not Result then
            FreeAndNil(vDbView);
          oDbView := vDbView;
        end;
      end;

    finally
      vResource.SelectQR.Close;
    end;
  except
    Result := False;
  end;
  if result then
  begin
    vResource.ProgCheckQR.ParamByName('position_id').AsInteger := APositionID;
    vResource.ProgCheckQR.Open;
    try
      vDbExt.InProgQueueState.pqState := (vResource.ProgCheckQR.RecordCount > 0);
      if vDbExt.InProgQueueState.pqState then
      begin
        vDbExt.InProgQueueState.pqID := vResource.ProgCheckQR.FieldByName('q_id').AsInteger;
        vDbExt.InProgQueueState.pqResult := byte(vResource.ProgCheckQR.FieldByName('result').AsInteger);
      end;
    finally
      vResource.ProgCheckQR.Close;
    end;
  end;
end; exports PositionViewSelectionExec;

procedure DisposeSelectionResource(var AResource : Pointer); stdcall;
var
vResource : pSelectionResource;
begin
  vResource := pSelectionResource(AResource);
  FreeAndNil(vResource.SelectQR);
  FreeAndNil(vResource.ProgCheckQR);
  FreeAndNil(vResource.PosData);
  vResource.DMAccess := nil;
  Dispose(vResource);
end; exports DisposeSelectionResource;

procedure PositionsDBInitProc(const ADMAccess : IDMAccess; ABoardSerial : Word; const APositionList : IPositionsList);stdcall;
var
i : Word;
vNeedDelete : Boolean;
vPosition : IPosition;
vPosExtentions : IExtentions;
vChipAbstract : TMilandrRev8Registers;
vDBExt : TStp8milDbExtention;
vDB : TEmbSQLDataBase;
vPosData : TMemIniFile;
vConnection : TvdConnection;
vQR, ProgCheckQR : TvdQuery;
vFormatSettings: TFormatSettings;
begin
  if (APositionList.Count < 1) then
    Exit;
  vConnection := nil;
  if (TThread.Current.ThreadID <> MainThreadID) then
  begin
    vConnection := ADMAccess.CreateConnection(nil);
    vConnection.Connected := True;
  end;
  vQR := ADMAccess.CreateReadQuery(nil, 'SELECT RPOS.ID, RPOS.RNUM, RPOS.STATUS, RPOS.RCARD '+
        'FROM RPOS  WHERE ((RPOS.BOARD = :BOARD) AND (RPOS.BOARDPOS = :BOARDPOS) '+
        'AND (RPOS.STATUS <= 1)) ORDER BY RPOS.ID', vConnection);

  ProgCheckQR := ADMAccess.CreateReadQuery(nil, 'SELECT PROG_QUE.Q_ID, PROG_QUE.POSITION_ID, '+
    'PROG_QUE.RESULT FROM PROG_QUE WHERE (POSITION_ID = :POSITION_ID)',
                                          vConnection);
  vPosData := TMemIniFile.Create('');
  vFormatSettings := TFormatSettings.Create(1033);
  try
    vQR.Prepare;
    ProgCheckQR.Prepare;
    vQR.ParamByName('BOARD').AsInteger := ABoardSerial;
    for i := APositionList.Count -1 downto 0 do
    begin
      vPosition := nil;
      vChipAbstract := nil;
      vPosExtentions := nil;
      vDBExt := nil;
      vPosData.Clear;
      if APositionList.QueryItem(i, IPosition, vPosition)
      and Supports(vPosition, IExtentions, vPosExtentions) then
      begin

        if vPosExtentions.Find(TMilandrRev8Registers, vChipAbstract) then
        begin
          vPosExtentions.Install(TStp8milDbExtention, vDBExt);
          if Assigned(vDBExt) then
          try
            vNeedDelete := true;
            vQR.ParamByName('BOARDPOS').AsSmallInt := vPosition.BoardPos;
            try
              vQR.Open;
              try
                vNeedDelete := (vQR.RecordCount < 1);
                if not vNeedDelete then
                begin
                  vNeedDelete := false;
                  while not vQR.eof do
                  try
                    vDBExt.fBaseID := vQR.FieldByName('ID').AsInteger;
                    vDBExt.fRCardID :=  vQR.FieldByName('RCARD').AsInteger;
                    vDBExt.fSerialNum := vQR.FieldByName('RNUM').AsString;
                    vDBExt.fBoardState := Byte(vQR.FieldByName('STATUS').AsInteger);
                    vDBExt.fBoard_SN := ABoardSerial;
                    vDBExt.fBoard_Pos := vPosition.BoardPos;
                    vDB := vDBExt.CreateConnection(ADMAccess.DBHomePath+
                                                  IntToStr(vDBExt.fRCardID)+
                                                  '\'+vDBExt.fSerialNum+'.'+C_ResFileExt);
                    try
                      if (not vDBExt.ReadPosData(vDB))
                      or (not vDBExt.ReadPosData(vDB, vPosData))
                      or (not vDBExt.ReadPosData(vPosData, vFormatSettings))
                      or (not IsEqualGUID(vDBExt.ChipGUID, MilandrRev8.Consts.C_GUID)) then
                        vNeedDelete := True
                      else
                      begin
                        if vDBExt.FatalError then
                        begin
                          vDBExt.FatalError := false;
                          vDBExt.UpdatePosData(vDB, []);
                        end;
                      end;
                    finally
                      if vDB.TransactionActive then
                        vDB.RollBack;
                      vDB.DBClose;
                      FreeAndNil(vDB);
                    end;
                    if vNeedDelete then
                      Break;
                    vChipAbstract.Assign(vDBExt.InitRegisters);
                    ProgCheckQR.ParamByName('POSITION_ID').AsInteger := vDBExt.fBaseID;
                    ProgCheckQR.Open;
                    vDbExt.InProgQueueState.pqState := (ProgCheckQR.RecordCount > 0);
                    if vDbExt.InProgQueueState.pqState then
                    begin
                      vDbExt.InProgQueueState.pqID := ProgCheckQR.FieldByName('Q_ID').AsInteger;
                      vDbExt.InProgQueueState.pqResult := byte(ProgCheckQR.FieldByName('RESULT').AsInteger);
                    end;
                    ProgCheckQR.Close;
                  finally
                    vQR.Next;
                  end;
                end;
              finally
                vQR.Close;
              end;
            except
              if vNeedDelete then
                APositionList.Delete(i);
              Continue;
            end;
            if vNeedDelete then
              APositionList.Delete(i);
          except
            APositionList.Delete(i);
          end else
            APositionList.Delete(i);
        end else
          APositionList.Delete(i);
      end;
    end;
    vQR.Close;
    ProgCheckQR.Close;
  finally
    vPosition := nil;
    vChipAbstract := nil;
    vPosExtentions := nil;
    vDBExt := nil;
    FreeAndNil(vQR);
    FreeAndNil(ProgCheckQR);
    FreeAndNil(vPosData);
    if Assigned(vConnection) then
    begin
      ADMAccess.CloseConnection(vConnection);
      FreeAndNil(vConnection);
    end;
  end;
end; exports PositionsDBInitProc;

type
TAddMeasRecord = record
  BitIdx : Byte;
  Value : SmallInt;
end;
TAddMeasPattern = class(TList<TAddMeasRecord>)
  public
  BaseID : Integer;
  procedure Load(const ADMAccess : IDMAccess; const AConnection : TvdConnection; ABaseID : Integer);
end;

procedure TAddMeasPattern.Load(const ADMAccess : IDMAccess; const AConnection : TvdConnection; ABaseID : Integer);
const
C_Sel_Pattern = 'SELECT PROC_PATTERNS.PATTERN_ID, PROC_PATTERNS.DATA_TEXT '+
'FROM PROC_PATTERNS  WHERE PROC_PATTERNS.PATTERN_ID = :ID';
var
vBitsIdx : TChipBytes;
vQR : TvdQuery;
vSt : TStringList;
vFile : TMemIniFile;
vRec : TAddMeasRecord;
vSectIdx, vArrIdx : Word;
vAddValue : SmallInt;
begin
  BaseID := ABaseID;
  Self.Clear;
  vQR:= ADMAccess.CreateReadQuery(nil, C_Sel_Pattern, AConnection);
  vSt := TStringList.Create;
  vFile := TMemIniFile.Create('');
  ReadControlledBitsIndex(TMilandrRev8Registers, 'Stp8AddMeasRegisters', False, vBitsIdx);
  try
    try
      vQR.Prepare;
      vQR.ParamByName('id').AsInteger := ABaseID;
      vQR.Open;
      if (vQR.RecordCount > 0) and (Length(vBitsIdx) > 0) then
      begin
        vSt.Clear;
        vSt.Text := vQR.FieldByName('data_text').AsWideString;
        vFile.SetStrings(vSt);
        vSt.Clear;
        vFile.ReadSections(vST);
        vSectIdx := 0;
        while vSectIdx < vST.Count do
        begin
          for vArrIdx := Low(vBitsIdx) to High(vBitsIdx) do
          begin
            if vFile.ValueExists(vST[vSectIdx], TMilandrRev8Registers.BitName(vBitsIdx[vArrIdx])) then
            begin
              vAddValue := vFile.ReadInteger(vST[vSectIdx], TMilandrRev8Registers.BitName(vBitsIdx[vArrIdx]), 0);
              if (vAddValue <> 0) then
              begin
                vRec.BitIdx := vBitsIdx[vArrIdx];
                vRec.Value := vAddValue;
                Add(vRec);
              end;
            end;
          end;
          Inc(vSectIdx);
        end;
      end;
      vQR.Close;
    except
      on e : exception do
        ADMAccess.OnException(vQR, E);
    end;
  finally
    FreeAndNil(vQR);
    FreeAndNil(vSt);
    FreeAndNil(vFile);
    SetLength(vBitsIdx, 0);
  end;
end;

procedure PrepareStatePositionsProc(const ADMAccess : IDMAccess;
                                const APositionList : IPositionsList;
                                AState : byte);stdcall;
var
i, vIdx : Word;
vPosExtentions : IExtentions;
vDBExt : TStp8milDbExtention;
vConnection : TvdConnection;
vCompensResult : TCompensationResult;
vQR : TvdQuery;
vDbFile : TEmbSQLDataBase;
vNewIterState : TIterStates;
vIterSaveFields : MilandrRev8.DboBase.TFieldOptions;
vPosLog : TPosLogItem;
vCardOptions : TStp8milProcessDBOptions;
vNewMeasSol : TMeasure<TSolution>;
vCardOptionsList : TObjectList<TStp8milProcessDBOptions>;
vAddMeasPattern : TAddMeasPattern;
vAddMeasPatterns : TObjectList<TAddMeasPattern>;
function GetCardOptions(ACardId : Integer; var oOptions : TStp8milProcessDBOptions) : boolean;
var
vIdx : Word;
begin
  Result := false;
  vIdx := 0;
  while vIdx < vCardOptionsList.Count do
  begin
    Result := vCardOptionsList[vidx].CardID = ACardId;
    if Result then
    begin
      oOptions := vCardOptionsList[vidx];
      break
    end;
    Inc(vIdx);
  end;
  if not result then
  begin
    oOptions := TStp8milProcessDBOptions.Create;
    Result := oOptions.Stp8_Load(ADMAccess, vConnection, ACardId);
    if result then
      vCardOptionsList.Add(oOptions)
    else
      FreeAndNil(oOptions);
  end;
end;
function GetAddMeasPattern(ABaseId : Integer; var oPattern : TAddMeasPattern) : boolean;
var
vIdx : Word;
begin
  Result := false;
  vIdx := 0;
  while vIdx < vAddMeasPatterns.Count do
  begin
    Result := vAddMeasPatterns[vidx].BaseID = ABaseId;
    if Result then
    begin
      oPattern := vAddMeasPatterns[vidx];
      break
    end;
    Inc(vIdx);
  end;
  if not result then
  begin
    oPattern := TAddMeasPattern.Create;
    vAddMeasPatterns.Add(oPattern);
    oPattern.Load(ADMAccess, vConnection, ABaseId);
    Result := True;
  end;
end;
begin
  if APositionList.Count < 1 then
    Exit;
  vConnection := nil;
  if (TThread.Current.ThreadID <> MainThreadID) then
  begin
    vConnection := ADMAccess.CreateConnection(nil);
    vConnection.Connected := True;
  end;
  try
    case AState of
      {$REGION 'StartTest'}
      0:
      begin
        vQR := ADMAccess.CreateWriteQuery(nil, 'UPDATE RPOS SET STATUS = 1 WHERE ID = :ID', vConnection);
        try
          vQR.Prepare;
          for i := 0 to APositionList.Count -1 do
          begin
            if APositionList.QueryItem(i, IExtentions, vPosExtentions)
            and vPosExtentions.Find(TStp8milDbExtention, vDBExt)
            and (vDBExt.fBoardState < 1) then
            begin
              vDBExt.fBoardState := 1;
              vQR.ParamByName('ID').AsInteger := vDBExt.BaseID;
              vQR.ExecSQL;
            end;
            vDBExt := nil;
            vPosExtentions := nil;
          end;
          try
            ADMAccess.Commit(vQR);
          except on E : Exception do
              ADMAccess.OnException(vQR, E);
          end;
        finally
          FreeAndNil(vQR);
        end;
      end;
      {$ENDREGION}
      {$REGION 'InRange'}
      2:
      begin
        vPosLog.Result := 'OK';
        vCardOptionsList := TObjectList<TStp8milProcessDBOptions>.Create;
        vAddMeasPatterns := TObjectList<TAddMeasPattern>.Create;
        try
          for i := 0 to APositionList.Count -1 do
          begin
            if APositionList.QueryItem(i, IExtentions, vPosExtentions)
            and vPosExtentions.Find(TStp8milDbExtention, vDBExt) then
            begin
              if vDBExt.NewIterRequest
              and GetCardOptions(vDBExt.RCardID, vCardOptions) then
              begin
                if vDBExt.ItersCount = 0 then
                begin
                  vDBExt.LastIter.Options.Assign(vDBExt.StartOptions);
                  vIterSaveFields := [foOptions, foRange];
                end else
                begin
                  vDBExt.LastIter.Options.Assign(vDBExt.LastIter.NextIterOptions);
                  vDBExt.LastIter.Errors.Clear;
                  vDBExt.LastIter.MeasPoints.Clear;
                  vDBExt.LastIter.State := [];
                  vDBExt.LastIter.Results.dF := 1E6;
                  vDBExt.LastIter.Results.dF25 := 1E6;
                  vDBExt.LastIter.Results.Iter_dF := 1E6;
                  vDBExt.LastIter.Results.Iter_dF25 := 1E6;
                  vDBExt.LastIter.Results.Calc_dF := 1E6;
                  vDBExt.LastIter.Results.Calc_dF25 := 1E6;
                  vIterSaveFields := [foOptions, foRange, foMeasCalc, foIterCalc, foResults, foNextOptions];
                end;
                vDBExt.LastIter.MeasPoints.CopyFrom(vCardOptions.ControlOptions.InPointOptions);
                if (rpFreqFinal in vDBExt.LastIter.Options.ControlOptions.InRangeOptions[vDBExt.LastIter.Options.ControlOptions.MeasureMode].s) then
                begin
                  vIdx := 0;
                  while vIdx < vDBExt.LastIter.MeasPoints.Count do
                  begin
                    if not vDBExt.LastIter.MeasPoints[vIdx].WorkPoint then
                      Include(vDBExt.LastIter.MeasPoints[vIdx].MeasSucc.s, rpFreqFinal);
                    Inc(vIdx);
                  end;
                  vDBExt.LastIter.CalcResults.Clear;
                  vDBExt.LastIter.MeasSolutions.Clear;
                end;
                if ([rpVvarCurrMeas, rpVvarCharactMeas, rpFreqCurr] <=
                vDBExt.LastIter.Options.ControlOptions.InRangeOptions[vDBExt.LastIter.Options.ControlOptions.MeasureMode].s) then
                begin
                  vDBExt.LastIter.MeasSolutions.Clear;
                  Include(vIterSaveFields, foMeas);
                  if (vDBExt.LastIter.CalcResults.Count > 0) then
                  begin
                    {перемещение из прошлой итерации}
                    vDBExt.LastIter.MoveCalcToMeas;
                    vDBExt.LastIter.CalcResults.Clear;
                  end else
                  begin
                    {заполнение листа измерений из начальных плюс паттерн дополнительных }
                    vNewMeasSol := vDBExt.LastIter.MeasSolutions.NewItem;
                    vNewMeasSol.Registers.Assign(vDBExt.InitRegisters);
                    if GetAddMeasPattern(vCardOptions.ControlOptions.AdditionalMeasurePatternID, vAddMeasPattern) then
                    begin
                      vIdx := 0;
                      while vIdx < vAddMeasPattern.Count do
                      begin
                        vNewMeasSol := vDBExt.LastIter.MeasSolutions.NewItem;
                        vNewMeasSol.Registers.Assign(vDBExt.InitRegisters);
                        vNewMeasSol.Registers.BitValue[vAddMeasPattern[vIdx].BitIdx] :=
                        vDBExt.InitRegisters.BitValue[vAddMeasPattern[vIdx].BitIdx] + vAddMeasPattern[vIdx].Value;
                        Inc(vIdx);
                      end;
                    end;
                  end;
                end;

                vDbFile := vDBExt.CreateConnection;
                try
                  if vDBExt.LastIter.CreateRecord(vDbFile)
                  and vDBExt.LastIter.Write(vDbFile, (vDBExt.ItersCount > 0), vIterSaveFields) then
                  begin
                    vPosLog.DT := Now;
                    vPosLog.Step := Format('Создание итерации %d',[vDBExt.LastIter.Num]);
                    if (rpFreqFinal in vDBExt.LastIter.Options.ControlOptions.InRangeOptions[vDBExt.LastIter.Options.ControlOptions.MeasureMode].s) then
                      vPosLog.Comment := 'Итерация финального прогона'
                    else
                      vPosLog.Comment := Format('Дополнительных измерений:%d',[vDBExt.LastIter.MeasSolutions.Count]);
                    vPosLog.Save(vDbFile);
                    vDBExt.ReadPosData(vDbFile);
                  end;
                finally
                  vDbFile.DBClose;
                  FreeAndNil(vDbFile);
                end;
              end;
            end;
            vDBExt := nil;
            vPosExtentions := nil;
          end;
        finally
          FreeAndNil(vCardOptionsList);
          FreeAndNil(vAddMeasPatterns);
        end;
      end;
      {$ENDREGION}
      {$REGION 'MeasureFinished'}
      3:
      begin
        for i := 0 to APositionList.Count -1 do
        begin
          if APositionList.QueryItem(i, IExtentions, vPosExtentions)
          and vPosExtentions.Find(TStp8milDbExtention, vDBExt)
          and (vDBExt.ItersCount > 0)
          and (not (isSucc in vDBExt.LastIter.State)) then
          begin
            vDbFile := vDBExt.CreateConnection;
            try
              if (vDBExt.LastIter.MeasPoints.SuccCount = vDBExt.LastIter.MeasPoints.Count) then
              begin
                vNewIterState := vDBExt.LastIter.State;
                Include(vNewIterState, isSucc);
                vDBExt.LastIter.State := vNewIterState;
                vDBExt.LastIter.Results.FinalizeTime := now;
                if vDBExt.LastIter.Write(vDbFile, True, [foResults]) then
                begin
                  vPosLog.DT := Now;
                  vPosLog.Step := Format('Итерация %d завершена',[vDBExt.LastIter.Num]);
                  vPosLog.Result := 'OK';
                  vPosLog.Comment := '';
                  vPosLog.Save(vDbFile);
                end;
              end else
              begin
                vPosLog.DT := Now;
                vPosLog.Step := Format('Одна или более точек в итерации %d не измерены',[vDBExt.LastIter.Num]);
                vPosLog.Result := 'Внимание';
                vPosLog.Comment := '';
                vPosLog.Save(vDbFile);
              end;
            finally
              vDbFile.DBClose;
              FreeAndNil(vDbFile);
            end;
          end;
          vDBExt := nil;
          vPosExtentions := nil;
        end;
      end;
      {$ENDREGION}
    end;
  finally
    if Assigned(vConnection) then
    begin
      ADMAccess.CloseConnection(vConnection);
      FreeAndNil(vConnection);
    end;
  end;
end; exports PrepareStatePositionsProc;

function PositionsDBCheckID(const APositionList : IPositionsList;
                          const AID : integer):boolean;stdcall;
var
i : Word;
vPosExtentions : IExtentions;
vDBExt : TPositionDBExtention;
begin
  Result := APositionList.Count > 0;
  if not Result then
    Exit;
  for i := 0 to APositionList.Count -1 do
  begin
    Result := (APositionList.QueryItem(i, IExtentions, vPosExtentions))
    and (vPosExtentions.Find(TPositionDBExtention, vDBExt));
    if Result then
      Result := (vDBExt.BaseID = AID);
    vDBExt := nil;
    vPosExtentions := nil;
    if Result then
      Break;
  end;
end; exports PositionsDBCheckID;

procedure PositionsDBDeleteLastIter(const ADMAccess : IDMAccess;
                                const APositionList : IPositionsList;
                                ACheckActive : boolean);stdcall;
var
i : Word;
vPosExtentions : IExtentions;
vPosition : IPosition;
vDBExt : TPositionDBExtention;
vDB : TEmbSQLDataBase;
vLog : TPosLogItem;
begin
  if APositionList.Count < 1 then
    Exit;
  if ACheckActive then
  begin
    for i := 0 to APositionList.Count -1 do
    begin
      if APositionList.QueryItem(i, IPosition, vPosition)
      and vPosition.Active
      and Supports(vPosition, IExtentions, vPosExtentions)
      and vPosExtentions.Find(TPositionDBExtention, vDBExt)
      and (vDBExt.ItersCount > 0) then
      begin
        vDB := vDBExt.CreateConnection;
        try
          if vDBExt.DefaultIterationClass.Delete(vDB, vDBExt.ItersCount) then
          begin
            vLog.DT := Now;
            vLog.Step := 'Delete uncompleted iteration';
            vLog.Result := 'OK';
            vLog.Comment := '';
            vLog.Save(vDB);
            vDBExt.ReadPosData(vDB);
          end;
        finally
          if vDB.TransactionActive then
            vDB.RollBack;
          vDB.DBClose;
          FreeAndNil(vDB);
        end;
      end;
      vDBExt := nil;
      vPosExtentions := nil;
      vPosition := nil;
    end;
  end else
  begin
    for i := 0 to APositionList.Count -1 do
    begin
      if APositionList.QueryItem(i, IExtentions, vPosExtentions)
      and vPosExtentions.Find(TPositionDBExtention, vDBExt)
      and (vDBExt.ItersCount > 0) then
      begin
        vDB := vDBExt.CreateConnection;
        try
          if vDBExt.DefaultIterationClass.Delete(vDB, vDBExt.ItersCount) then
            vDBExt.ReadPosData(vDB);
        finally
          if vDB.TransactionActive then
            vDB.RollBack;
          vDB.DBClose;
          FreeAndNil(vDB);
        end;
      end;
      vDBExt := nil;
      vPosExtentions := nil;
    end;
  end;
end; exports PositionsDBDeleteLastIter;

procedure PositionsDBDeleteUnSuccIter(const ADMAccess : IDMAccess;
                                const APositionList : IPositionsList;
                                ACheckActive : boolean);stdcall;
var
i : Word;
vPosExtentions : IExtentions;
vPosition : IPosition;
vDBExt : TPositionDBExtention;
vDB : TEmbSQLDataBase;
vState : TIterStates;
vLog : TPosLogItem;
begin
  if APositionList.Count < 1 then
    Exit;
  if ACheckActive then
  begin
    for i := 0 to APositionList.Count -1 do
    begin
      if APositionList.QueryItem(i, IPosition, vPosition)
      and vPosition.Active
      and Supports(vPosition, IExtentions, vPosExtentions)
      and vPosExtentions.Find(TPositionDBExtention, vDBExt)
      and (vDBExt.ItersCount > 0) then
      begin
        vDB := vDBExt.CreateConnection;
        try
          if (vDBExt.DefaultIterationClass.ReadState(vDB, vDBExt.ItersCount, vState))
          and (not (isSucc in vState))
          and (vDBExt.DefaultIterationClass.Delete(vDB, vDBExt.ItersCount)) then
          begin
            vLog.DT := Now;
            vLog.Step := 'Удаление незавершенной итерации';
            vLog.Result := 'OK';
            vLog.Comment := '';
            vLog.Save(vDB);
            vDBExt.ReadPosData(vDB);
          end;
        finally
          if vDB.TransactionActive then
            vDB.RollBack;
          vDB.DBClose;
          FreeAndNil(vDB);
        end;
      end;
      vDBExt := nil;
      vPosExtentions := nil;
      vPosition := nil;
    end;
  end else
  begin
    for i := 0 to APositionList.Count -1 do
    begin
      if APositionList.QueryItem(i, IExtentions, vPosExtentions)
      and vPosExtentions.Find(TPositionDBExtention, vDBExt)
      and (vDBExt.ItersCount > 0) then
      begin
        vDB := vDBExt.CreateConnection;
        try
          if (vDBExt.DefaultIterationClass.ReadState(vDB, vDBExt.ItersCount, vState))
          and (not (isSucc in vState))
          and (vDBExt.DefaultIterationClass.Delete(vDB, vDBExt.ItersCount)) then
            vDBExt.ReadPosData(vDB);
        finally
          if vDB.TransactionActive then
            vDB.RollBack;
          vDB.DBClose;
          FreeAndNil(vDB);
        end;
      end;
      vDBExt := nil;
      vPosExtentions := nil;
    end;
  end;

end; exports PositionsDBDeleteUnSuccIter;


function TStp8milProcessDBOptions.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TStp8milProcessDBOptions.Stp8_Load(const ADMAccess: IDMAccess; const AConnection : TvdConnection;
  const ACardID: Integer; const ATextOptions : TStrings): boolean;
var
vTextOptQR, vSelectQR, vSelPattern : TvdQuery;
vIpOp : TInPointOption;
vIniFile : TMemIniFile;
vSt : TStringList;
begin
  Result := false;
  vTextOptQR := nil;
  if not Assigned(ATextOptions) then
  begin
    vTextOptQR := ADMAccess.CreateReadQuery(nil, 'SELECT PROC_OPTIONS  '+
    'FROM RCARD WHERE RCARD.ID = :ID', AConnection);
  end  else
    Load(ATextOptions);
  if not Assigned(ADMAccess) then
    Exit;
  CardID := ACardID;
  vSelectQR := ADMAccess.CreateReadQuery(nil, C_SelCardCurrPattern, AConnection);
  vSelPattern := ADMAccess.CreateReadQuery(nil, C_SelCurrPointPattern, AConnection);

  vIniFile := TMemIniFile.Create('');
  vSt := TStringList.Create;
  try
    try
      if Assigned(vTextOptQR) then
      begin
        vTextOptQR.Prepare;
        vTextOptQR.ParamByName('ID').AsInteger := ACardID;
        vTextOptQR.Open;
        Result := vTextOptQR.RecordCount > 0;
        if not Result then
          Exit;
        vSt.Clear;
        vSt.Text := vTextOptQR.FieldByName('PROC_OPTIONS').AsWideString;
        Load(vSt);
        vTextOptQR.Close;
      end;
      vSelectQR.Prepare;
      vSelectQR.ParamByName('RCARD_ID').AsInteger := ACardID;
      vSelectQR.ParamByName('PATTERN_TYPE').AsSmallInt := 1;
      vSelectQR.Open;
      if vSelectQR.RecordCount > 0 then
         ControlOptions.StartRegistersPatternID := vSelectQR.FieldByName('PATTERN_ID').AsInteger;
      vSelectQR.Close;
      {if not vSelectQR.Prepared then
        vSelectQR.Prepare; }
      vSelectQR.ParamByName('RCARD_ID').AsInteger := ACardID;
      vSelectQR.ParamByName('PATTERN_TYPE').AsSmallInt := 2;
      vSelectQR.Open;
      if vSelectQR.RecordCount > 0 then
         ControlOptions.AdditionalMeasurePatternID := vSelectQR.FieldByName('PATTERN_ID').AsInteger;
      vSelectQR.Close;
      {if not vSelectQR.Prepared then
        vSelectQR.Prepare;  }
      vSelectQR.ParamByName('RCARD_ID').AsInteger := ACardID;
      vSelectQR.ParamByName('PATTERN_TYPE').AsSmallInt := 3;
      vSelectQR.Open;
      if vSelectQR.RecordCount > 0 then
         CalcOptions.RegWeighPatternID := vSelectQR.FieldByName('PATTERN_ID').AsInteger;
      vSelectQR.Close;
      {if not vSelectQR.Prepared then
        vSelectQR.Prepare;  }
      vSelectQR.ParamByName('RCARD_ID').AsInteger := ACardID;
      vSelectQR.ParamByName('PATTERN_TYPE').AsSmallInt := 5;
      vSelectQR.Open;
      if vSelectQR.RecordCount > 0 then
         ControlOptions.VVFPatternID := vSelectQR.FieldByName('PATTERN_ID').AsInteger;
      vSelectQR.Close;
      vSelectQR.Unprepare;
      vSelectQR.Params.Clear;
      ControlOptions.InPointOptions.Clear;
      vSelectQR.SQL.Text := C_SelCardPoints;
      vSelectQR.Prepare;
      vSelPattern.Prepare;
      vSelectQR.ParamByName('RCARD_ID').AsInteger := ACardID;
      vSelectQR.Open;

      while not vSelectQR.Eof do
      begin
        try
          vIpOp.PointID := vSelectQR.FieldByName('POINT_ID').AsInteger;
          vIpOp.PointValue := vSelectQR.FieldByName('POINTVALUE').AsFloat;
          vIpOp.PatternExists := False;
          vSt.Clear;
          vIniFile.Clear;
          vSt.Text := vSelectQR.FieldByName('INI_OPTIONS').AsWideString;
          vIniFile.SetStrings(vSt);
          vIpOp.WorkPoint := vIniFile.ReadBool(MilandrRev8.Consts.C_GUID.ToString, 'WorkPoint', false);
          vSelPattern.ParamByName('POINT_ID').AsInteger := vIpOp.PointID;
          vSelPattern.ParamByName('PATTERN_TYPE').AsSmallInt := 4;
          vSelPattern.ParamByName('CHIP_GUID').AsString := MilandrRev8.Consts.C_GUID.ToString;
          try
            vSelPattern.Open;
            vIpOp.PatternExists := vSelPattern.RecordCount = 1;
            if vIpOp.PatternExists then
              vIpOp.PatternID := vSelPattern.FieldByName('PATTERN_ID').AsInteger;
            vSelPattern.Close;
          except
            on e : Exception do
            begin
              vIpOp.PatternExists := False;
              ADMAccess.OnException(vSelPattern, E);
              Break;
            end;
          end;
        finally
          ControlOptions.InPointOptions.Add(vIpOp);
          vSelectQR.Next;
        end;
      end;
      vSelectQR.Close;
      Result := True;
    except
      on e : Exception do
        ADMAccess.OnException(vSelectQR, E);
    end;
  finally
    if Assigned(vTextOptQR) then
    begin
      FreeAndNil(vTextOptQR);
    end;
    FreeAndNil(vSelPattern);
    FreeAndNil(vSelectQR);
    FreeAndNil(vIniFile);
    FreeAndNil(vSt);
  end;
end;

function TStp8milProcessDBOptions.Stp8_Save(const ADMAccess: IDMAccess; const AConnection : TvdConnection;
  const ACardID: Integer): boolean;
var
vSaveLinkProc : TvdStoredProc;
vSelectQR, vWriteQR : TvdQuery;
i, vUpdateCount, vOldpattern, vNewpattern: Integer;
vNeedUpdate : Boolean;
vIpOp : TInPointOption;
vIniFile : TMemIniFile;
vSt : TStringList;
begin
  Result := false;
  if not Assigned(ADMAccess) then
    Exit;
  vSelectQR := ADMAccess.CreateReadQuery(nil, C_SelCardCurrPattern, AConnection);
  vSaveLinkProc := ADMAccess.CreateWriteStoredProc(nil, 'UPDATE_CP_LINK', AConnection);
  vWriteQR := ADMAccess.CreateWriteQuery(nil, AConnection);
  vIniFile := TMemIniFile.Create('');
  vSt := TStringList.Create;
  try
    vSaveLinkProc.Prepare;
    vSaveLinkProc.ParamByName('CARDID').AsInteger := ACardID;
    Result := True;
    vUpdateCount := 0;
    vSelectQR.Prepare;
    for I := 1 to 4 do
    begin
      case i of
        1: vNewpattern := ControlOptions.StartRegistersPatternID;
        2: vNewpattern := ControlOptions.AdditionalMeasurePatternID;
        3: vNewpattern := CalcOptions.RegWeighPatternID;
        4: vNewpattern := ControlOptions.VVFPatternID;
      end;
      if i in [1..3] then
        vSelectQR.ParamByName('PATTERN_TYPE').AsSmallInt := i
      else
        vSelectQR.ParamByName('PATTERN_TYPE').AsSmallInt := 5;
      vSelectQR.ParamByName('RCARD_ID').AsInteger := ACardID;
      vSelectQR.Open;
      if vSelectQR.RecordCount > 0 then
      begin
        vOldpattern := vSelectQR.FieldByName('PATTERN_ID').AsInteger;
        vNeedUpdate := vOldpattern <> vNewpattern;
      end
      else
      begin
        vNeedUpdate := True;
        vOldpattern := vNewpattern;
      end;
      vSelectQR.Close;
      if not vNeedUpdate then
         Continue;
      vSaveLinkProc.ParamByName('OLD_PATTERN').AsInteger :=  vOldpattern;
      vSaveLinkProc.ParamByName('NEW_PATTERN').AsInteger := vNewpattern;
      try
        vSaveLinkProc.ExecProc;
        inc(vUpdateCount);
        Result := Result and True;
      except
        on e : Exception do
        begin
          Result := false;
          ADMAccess.OnException(vSaveLinkProc, e);
        end;
      end;
    end;
    vSelectQR.Close;
    vSelectQR.Unprepare;
    vSelectQR.Params.Clear;
    if (vUpdateCount > 0) then
      ADMAccess.Commit(vSaveLinkProc);

    vSaveLinkProc.Unprepare;
    vSaveLinkProc.Params.Clear;
    if not result then
      Exit;
    if (ControlOptions.InPointOptions.Count > 0) then
    begin
      vSelectQR.SQL.Text := 'SELECT PFPOINTS.INI_OPTIONS FROM PFPOINTS ' +
                          'WHERE POINT_ID = :POINT_ID ';
      vWriteQR.SQL.Text := 'UPDATE PFPOINTS SET PFPOINTS.INI_OPTIONS = :INI_OPTIONS '+
                          'WHERE POINT_ID = :POINT_ID';
      vSelectQR.Prepare;
      vWriteQR.Prepare;
      try
        for I := 0 to ControlOptions.InPointOptions.Count -1 do
        begin
          vIpOp := ControlOptions.InPointOptions[i];
          vSelectQR.ParamByName('point_id').AsInteger := vIpOp.PointID;
          vSelectQR.Open;
          vSt.Clear;
          vIniFile.Clear;
          vSt.Text := vSelectQR.FieldByName('ini_options').AsWideString;
          vSelectQR.Close;
          vIniFile.SetStrings(vSt);
          vIniFile.WriteBool(MilandrRev8.Consts.C_GUID.ToString, 'WorkPoint', vIpOp.WorkPoint);
          vSt.Clear;
          vIniFile.GetStrings(vSt);
          vWriteQR.ParamByName('POINT_ID').Asinteger := vIpOp.PointID;
          vWriteQR.ParamByName('INI_OPTIONS').AsWideMemo := vSt.Text;
          try
            vWriteQR.ExecSQL;
          except
            on e : Exception do
            begin
              Result := false;
              ADMAccess.OnException(vWriteQR, e);
            end;
          end;
        end;
        vSelectQR.Close;
        vSelectQR.Unprepare;
        vSelectQR.Params.Clear;
      finally
        ADMAccess.Commit(vWriteQR);
      end;
    end;
    if not result then
      Exit;

    if (ControlOptions.InPointOptions.Count > 0) then
    begin
      vUpdateCount := 0;
      vSaveLinkProc.StoredProcName := 'UPDATE_PP_LINK';
      vSelectQR.SQL.Text := 'SELECT PFPOINTS_PATTERN_LINKS.PATTERN_ID '+
                'FROM PFPOINTS_PATTERN_LINKS INNER JOIN PROC_PATTERNS '+
                'ON (PFPOINTS_PATTERN_LINKS.PATTERN_ID = PROC_PATTERNS.PATTERN_ID) '+
                'WHERE PFPOINTS_PATTERN_LINKS.PFPOINT_ID = :PFPOINT_ID '+
                'AND PROC_PATTERNS.PATTERN_TYPE = 4 '+
                'AND PROC_PATTERNS.CHIP_GUID = :CHIP_GUID';
      vSelectQR.Prepare;
      vSaveLinkProc.Prepare;
      for I := 0 to ControlOptions.InPointOptions.Count -1 do
      begin
        vIpOp := ControlOptions.InPointOptions[i];
        if not vIpOp.PatternExists then
           Continue;
        vNewpattern := vIpOp.PatternID;
        vSelectQR.ParamByName('PFPOINT_ID').AsInteger := vIpOp.PointID;
        vSelectQR.ParamByName('CHIP_GUID').AsString := MilandrRev8.Consts.C_GUID.ToString;
        vSelectQR.Open;
        if vSelectQR.RecordCount > 0 then
        begin
          vOldpattern := vSelectQR.FieldByName('PATTERN_ID').AsInteger;
          vNeedUpdate := vOldpattern <> vNewpattern;
        end
        else
        begin
          vNeedUpdate := True;
          vOldpattern := vNewpattern;
        end;
        vSelectQR.Close;
        if not vNeedUpdate then
           Continue;
        vSaveLinkProc.ParamByName('PFPOINT_ID').AsInteger := vIpOp.PointID;
        vSaveLinkProc.ParamByName('OLD_PATTERN').AsInteger :=  vOldpattern;
        vSaveLinkProc.ParamByName('NEW_PATTERN').AsInteger := vNewpattern;
        try
          vSaveLinkProc.ExecProc;
          inc(vUpdateCount);
          Result := Result and True;
        except
          on e : Exception do
          begin
            Result := false;
            ADMAccess.OnException(vSaveLinkProc, e);
          end;
        end;
      end;
      vSelectQR.Close;
      if (vUpdateCount > 0) then
        ADMAccess.Commit(vSaveLinkProc);
    end;
  finally
    vSaveLinkProc.Close;
    FreeAndNil(vSaveLinkProc);
    vWriteQR.Close;
    FreeAndNil(vWriteQR);
    FreeAndNil(vSelectQR);
    FreeAndNil(vIniFile);
    FreeAndNil(vSt);
  end;
end;


function TStp8milProcessDBOptions._AddRef: Integer;
begin
  result := -1;
end;

function TStp8milProcessDBOptions._Release: Integer;
begin
  result := -1;
end;

{ TMilPositionDBView }


class function TMilPositionDBView.PositionDBExtentionClass: TPositionDBExtentionClass;
begin
  result := TStp8milDbExtention;
end;

const
 C_LegendInfoColor : array [TLegendInfo] of TColor =
 {      liEmpty,        liRCMask,        liCreated,}
 (TColors.SysBtnFace, TColors.Silver, TColors.SysHighlight,
 {liPrepare,         liInWork,       liMeasComplete,    liCompensError}
 TColors.SysInfoBk,  TColors.Yellow,  TColors.Aqua,      $00877AFE,
 {liReadyForProg, liProgOK, liProgError, }
    $00B1F54B,    { $0080FF80,} $00FF80FF,
 {liFinalTest,   liFinaTestOK,     liFinalTestErr,  liOtherErr }
    $000BF4B5,    TColors.Lime,     $00877AFE,     TColors.Red);

function TMilPositionDBView.InProgMode: Boolean;
var
vDbExt : TStp8milDbExtention;
begin
  vDbExt := DbExtention as TStp8milDbExtention;
  Result := vDbExt.InProgQueueState.pqState;
end;

function TMilPositionDBView.LastIterSucc: Boolean;
var
vDbExt : TStp8milDbExtention;
begin
  vDbExt := DbExtention as TStp8milDbExtention;
  Result := (vDbExt.ItersCount > 0) and (isSucc in vDbExt.LastIter.State);
end;

class procedure TMilPositionDBView.ListLegendFiller(
  const AList: TList<TLegendItem>);
var
E: TLegendInfo;
vNewItem : TLegendItem;
begin
  AList.Clear;
  for E := Low(E) to High(E) do
  begin
     vNewItem.Name := C_LegendInfoText[E];
     vNewItem.Color := C_LegendInfoColor[E];
     AList.Add(vNewItem);
  end;
end;

procedure TMilPositionDBView.UpdateLegendValues(SelectedCardId: Integer;
  ACardSelected, ByCardMask: Boolean; const ACheckedList: TList<Boolean>);
var
vDbExt : TStp8milDbExtention;
vListOptions : TLegendOptions;
vIdx : Byte;
begin
  inherited;
  vListOptions := [];
  fHided := False;
  vDbExt := DbExtention as TStp8milDbExtention;
  for vIdx := 0 to ACheckedList.Count -1 do
  begin
    if ACheckedList[vIdx] then
      Include(vListOptions, TLegendInfo(vIdx));
  end;

  if (ACardSelected)
  and (ByCardMask)
  and (SelectedCardId <> vDbExt.RCardID) then
  begin
    fLegendState := liRCMask;
    fHided := not (liRCMask in vListOptions);
  end else
  begin
    if (DbExtention.BoardState = 0) then
    begin
      fLegendState := liCreated;
      fHided := not (liCreated in vListOptions);
    end else
    begin
      if vDbExt.InProgQueueState.pqState then
      begin
        case vDbExt.InProgQueueState.pqResult of
          0: fLegendState := liReadyForProg;
          1: fLegendState := liProgError;
          else
           fLegendState := liProgError;
        end;
      end else
      begin
        if (vDbExt.ItersCount > 0) then
        begin
          if vDbExt.LastIter.State <> [] then
          begin
            if (isCalcFail in vDbExt.LastIter.State) then
            begin
              fLegendState := liCompensError;
            end else
            begin
              fLegendState := liMeasComplete;
              if (vDbExt.LastIter.Options.ControlOptions.MeasureMode = pmFinalTest) then
              begin
                if (isSucc in vDbExt.LastIter.State)
                and (vDbExt.CompensationResult.ClassNum > 0) then
                  fLegendState := liFinaTestOK
                else
                  fLegendState := liFinalTestErr;
                {fLegendState := liFinalTest;
                {if (mrsFinalTestOk in vDbExt.StepsResults) then
                  fLegendState := liFinaTestOK
                else if (mrsFinalTestFail in vDbExt.StepsResults) then
                  fLegendState := liFinalTestErr; }
              end else if (vDbExt.LastIter.CalcResults.Count < 1) then
              begin
                fLegendState := liCompensError;
              end;
            end;
          end else
          begin
            if (vDbExt.fLastIter.Options.ControlOptions.MeasureMode = pmFinalTest) then
              fLegendState := liFinalTest
            else
              fLegendState := liInMeasure;
          end;
        end else
        begin
          fLegendState := liPrepare;
          if ([msrFreqTestFail, msrInfTrimFail, msrCDACTrimFail]  <= vDbExt.StepsResults) then
            fLegendState := liFatalErr;
        end;
      end;
      if vDbExt.FatalError then
        fLegendState := liFatalErr;
      fHided := not (fLegendState in vListOptions);
    end;
  end;

  if not fHided then
  begin
    fLegendColor := C_LegendInfoColor[fLegendState];

    if fLegendState <> liCreated then
    begin
      if (vDbExt.ItersCount > 0)
      and (vDbExt.LastIter.Options.ControlOptions.MeasureMode = pmFinalTest)
      and (isCalcOk in vDbExt.LastIter.State)
      and (fLegendState = liFinaTestOK)
      and (vDbExt.CompensationResult.ClassNum > 0) then
        fLegendString := Format('%s %d(%s)',
          [C_LegendInfoText[fLegendState],
          vDbExt.CompensationResult.ClassNum,
          vDbExt.CompensationResult.ClassName])
      else
        fLegendString := Format('%s',[C_LegendInfoText[fLegendState]]);
    end else
      fLegendString := C_LegendPositionCreated;
  end;
end;

{ TStp8masDbExtention }

procedure TStp8milDbExtention.AfterCreate;
begin
  inherited;
  InProgQueueState.pqState := False;
  fLastIter := TMilIteration<TSolution, TMeasure<TSolution>>.Create(self);
end;

procedure TStp8milDbExtention.BeforeDestroy;
begin
  inherited;
  FreeAndNil(fLastIter);
end;

function TStp8milDbExtention.DeletePosition(const AConnection: TvdConnection; const ADMAccess: IDMAccess): boolean;
var
vDelQR : TvdQuery;
begin
  result := false;
  vDelQR := ADMAccess.CreateWriteQuery(nil, 'delete from rpos where id = :id', AConnection);
  try
    vDelQR.Prepare;
    try
      vDelQR.ParamByName('id').AsInteger := self.BaseID;
      vDelQR.ExecSQL;
      if FileExists(MeasFileName)
      and not DeleteFile(MeasFileName)then
      begin
        vDelQR.UpdateTransaction.Rollback;
        exit;
      end;
      ADMAccess.Commit(vDelQR);
      result := true;
    except on E : Exception do
      begin
        result := false;
        ADMAccess.OnException(vDelQR, E);
        exit;
      end;
    end;
  finally
    FreeAndNil(vDelQR);
  end;
end;

function TStp8milDbExtention.ReadPosData(const ADB: TEmbSQLDataBase): Boolean;
begin
  result := inherited ReadPosData(ADB);
  if result and (ItersCount > 0) then
    result := fLastIter.Read(ADB, ItersCount);
end;

function TStp8milDbExtention.RecostructionRequest: boolean;
begin
  result := (ItersCount > 0)
    and ([isSucc] = fLastIter.State)
    and (arCalc in fLastIter.Options.ControlOptions.AfterCompensOptions.s);
end;

type
TMasStepsResultsAdapter = record
  case integer of
  0: (s: TMilStepsResults);
  1: (i: Cardinal);
end;

function TStp8milDbExtention.GetStepsResults: TMilStepsResults;
var
vAd : TMasStepsResultsAdapter;
begin
  vAd.i := fStepsResults;
  Result := vAd.s;
end;

procedure TStp8milDbExtention.SetStepsResults(const Value: TMilStepsResults);
var
vAd : TMasStepsResultsAdapter;
begin
  vAd.i := 0;
  vAd.s := Value;
  fStepsResults := vAd.i;
end;

procedure TStp8milDbExtention.StepResultExclude(Value: TMilStepsResult);
var
vTmp : TMilStepsResults;
begin
  vTmp := StepsResults;
  Exclude(vTmp, Value);
  StepsResults := vTmp;
end;

procedure TStp8milDbExtention.StepResultInclude(Value: TMilStepsResult);
var
vTmp : TMilStepsResults;
begin
  vTmp := StepsResults;
  Include(vTmp, Value);
  StepsResults := vTmp;
end;

function TStp8milDbExtention.NewIterRequest: boolean;
begin
  result := not InProgQueueState.pqState;
  if result then
  begin
    if (ItersCount = 0) then
    begin
      result := (StartOptions.ControlOptions.InRangeOptions[StartOptions.ControlOptions.MeasureMode].s <> []);
      exit;
    end else
    begin
      result := (isSucc in LastIter.State)
      and (LastIter.NextIterOptions.ControlOptions.InRangeOptions[LastIter.NextIterOptions.ControlOptions.MeasureMode].s <> []);
      if result and (LastIter.NextIterOptions.ControlOptions.MeasureMode = pmMeas) then
          result := (LastIter.CalcResults.Count > 0);
    end;
  end;
end;

procedure TStp8milDbExtention.SetFatalError(Value: boolean);
begin
  fFatalError := Value;
end;

procedure TStp8milDbExtention.SetNominal(const Value: double);
begin
  fNominal := Value;
end;

end.
