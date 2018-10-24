unit MilandrRev8.DboMain;

interface
  uses ChipAbstract, MilandrRev8IMS, Stp8PositionDBExtention, MilandrRev8.DboBase,
  MeasDataList, MilandrRev8.Matrix, VvarMatrix,
  System.Generics.Collections;

  type
  TFreqCurrObj = class(TMeasDataItem)
   strict private
    fDisp : Double;
    fVdd,
    fVC : double;
   private
    function GetDisp: Double;
    procedure SetDisp(const Value: Double);
    function GetItemErrors : TFMErrors;
    function GetVdd: double;
    function GetVC: double;
    procedure SetVC(const Value: double);
    procedure SetVdd(const Value: double);
   public
    property Disp : Double read GetDisp write SetDisp;
    property Vdd : double read GetVdd write SetVdd;
    property VC : double read GetVC write SetVC;
    property ItemErrors : TFMErrors read GetItemErrors;
  end;



  TVvarMCObj = class(TMeasDataItem)
   private
    fVdd,
    fVC : double;
    function GetItemErrors : TVCErrors;
    function GetVdd: double;
    function GetVC: double;
    procedure SetVC(const Value: double);
    procedure SetVdd(const Value: double);
   public
    property Vdd : double read GetVdd write SetVdd;
    property VC : double read GetVC write SetVC;
    property ItemErrors : TVCErrors read GetItemErrors;
  end;

  TFCPoint= class(TMeasDataItem)
   strict private
    fFreqCurve : TMeasDataList;
   protected
    class procedure EndUpdate(ADataList : TMeasDataList); override;
    class function CalcErrors(ADataList : TMeasDataList) : word; override;
    class function CalcTotalCount(ADataList : TMeasDataList) : word; override;
   public
    constructor Create(ADataList : TMeasDataList); override;
    destructor Destroy; override;
    function FreqCurve : TMeasDataList;
    function ErrorsExists(AErrors : TVFErrors):Boolean;
  end;

  TFCitem = class(TMeasDataItem)
   strict private
    fFreqDispPpm,
    fVdd,
    fVC : double;
    fRegisters : TMilandrRev8Registers;
   private
    function GetVdd : double;
    function GetVC : double;
    procedure SetVdd(AValue: double);
    procedure SetVC(AValue: double);
    function GetDisp: double;
    procedure SetDisp(AValue: double);
    function GetRegisters :TMilandrRev8Registers;
    function GetItemErrors : TVFErrors;
    procedure SetFcError(Value : TVFErrors);
    procedure ErrorInclude(AError : TVFError);
    procedure ErrorExclude(AError : TVFError);
   public
    constructor Create(ADataList : TMeasDataList); override;
    destructor Destroy; override;
    property DispPpm: double  read GetDisp  write SetDisp;
    property Vdd: double  read GetVdd write SetVdd;
    property VC: double  read GetVC write SetVC;
    property Registers : TMilandrRev8Registers read GetRegisters;
    property ItemErrors : TVFErrors read GetItemErrors write SetFcError;
  end;


  TTC_OFFObj = class(TMeasDataItem)
  strict private
    fDisp : Double;
    fVdd,
    fVC : double;
  private
    function GetItemErrors : TTOErrors;
    function GetDisp: Double;
    procedure SetDisp(const Value: Double);
    function GetVdd: double;
    function GetVC: double;
    procedure SetVC(const Value: double);
    procedure SetVdd(const Value: double);
  public
    property Disp : Double read GetDisp write SetDisp;
    property Vdd : double read GetVdd write SetVdd;
    property VC : double read GetVC write SetVC;
    property ItemErrors : TTOErrors read GetItemErrors;
  end;

  TFinalTestObj = class(TMeasDataItem)
   strict private
    fFreqDisp : Double;
    fVdd,
    fVC : double;
   private
    function GetItemErrors : TFTErrors;
    function GetFreqDisp: Double;
    procedure SetFreqDisp(const Value: Double);
    function GetVdd: double;
    function GetVC: double;
    procedure SetVC(const Value: double);
    procedure SetVdd(const Value: double);
   public
    property FreqDisp : Double read GetFreqDisp write SetFreqDisp;
    property Vdd : double read GetVdd write SetVdd;
    property VC : double read GetVC write SetVC;
    property ItemErrors : TFTErrors read GetItemErrors;
  end;

  TFCMeasResults = class
   private
    fCurves, fForce : TMeasDataList;
   public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Curves : TMeasDataList read fCurves;
    property Force : TMeasDataList read fForce;
  end;

  TVvarFCObj = class(TMeasDataItem)
   private
    function GetItemErrors : TVFErrors;
    procedure ErrorInclude(AError: TVFError);
   public
    property ItemErrors : TVFErrors read GetItemErrors;
  end;

  TCalcErrorObj = class(TMeasDataItem)
   private
    fMvError, fFreq : Double;
    function GetMVolts : Double;
    procedure SetMVolts(AValue : Double);
    procedure SetFreq(AValue : Double);
    function GetFreq : Double;
   protected
    class procedure EndUpdate(ADataList : TMeasDataList); override;
   public
    property MVolts :Double read GetMVolts write SetMVolts;
    function MVoltsX(AX : Double) :Double;
    property Freq :Double read GetFreq write SetFreq;
    function FreqX(AX : Double) :Double;
  end;
  TCalcCurves = class;
  TMeasureSolutionCurves<T : TCalcCurves> = class;

  TMilIterationResults  = class;
  TCalcCurves = class(TSolution)
   private
    fIterationMil: TMilIterationResults;
    fRootObj : TMeasureSolutionCurves<TCalcCurves>;
    fVvar,
    fFreq : TMeasDataList;
    fdReg, fPrior : Double;
   public
    constructor Create(const AIteration : TMilIterationBase; const ARootIdx : Byte);override;
    destructor Destroy; override;
    property IterationMil : TMilIterationResults read fIterationMil;
    property RootObj : TMeasureSolutionCurves<TCalcCurves> read fRootObj;
    procedure CalcCurves;overload;
    procedure CalcCurves(const ARegisters :TMilandrRev8Registers);overload;
    property Vvar : TMeasDataList read fVvar;
    property Freq : TMeasDataList read fFreq;
    property dReg : Double read fdReg;
    property Prior : Double read fPrior;
  end;


  TMeasureSolutionCurves<T : TCalcCurves> = class(TMeasure<T>)
   private
    fIterationMas: TMilIterationResults;
    fVvar,
    fFreq : TMeasDataList;
    fVvarMatrix : TVvarMatrix;
    fCharactTable : TCTMilRev8;
   public
    constructor Create(const AIteration : TMilIterationBase);override;
    destructor Destroy; override;
    property IterationMas : TMilIterationResults read fIterationMas;
    procedure Load(const ADB: TEmbSQLDataBase; ASolIdx : Word);
    property Vvar : TMeasDataList read fVvar;
    property Freq : TMeasDataList read fFreq;
    property Matrix : TVvarMatrix read fVvarMatrix;
    property CharactTable : TCTMilRev8 read fCharactTable;
  end;


  TMilIterationResults = class(TMilIteration<TCalcCurves, TMeasureSolutionCurves<TCalcCurves>>)
   private
    fFCResults : TFCMeasResults;
    fTC_OFF,
    fFreqFinal : TMeasDataList;
    fTC_OFFMaxDev : Double;
    procedure CalcIterStab(ACurvePrev, ACurveCurr: TMeasDataList);
   public
    constructor Create(const ADBExt : TPositionDBExtention);override;
    destructor Destroy; override;
    procedure Load(const ADB: TEmbSQLDataBase; ACalcResults : Boolean = false);
    procedure RecalcMeasureResults;
    property FCResults : TFCMeasResults read fFCResults;
    property FreqFinal : TMeasDataList read fFreqFinal;
    property TC_OFF : TMeasDataList read fTC_OFF;
    property TC_OFFMaxDev : Double read fTC_OFFMaxDev;
    function TotalErrorPercens : double;
  end;

  TMilResultsDBObj = class(TmilDbExtention)
   private
    fIterations : TObjectList<TMilIterationResults>;
   protected
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
   public
    class function DefaultIterationClass : TIterationClass;override;
    procedure Load(const AFileName : string; ACalcResults : Boolean = false);
    property Iterations : TObjectList<TMilIterationResults> read fIterations;
  end;

  procedure FCFill(const ADB: TEmbSQLDataBase; const ADest : TMeasDataList; AIter, ASolution: word);
  procedure VCFill(const ADB: TEmbSQLDataBase; const ADest : TMeasDataList; AIter, ASolution: word);
  procedure CharactFill(const ADB: TEmbSQLDataBase; const ATable : TVvarMatrix; AIter, ASolution: word);


  function FCErrorsStr(AErrors : TFMErrors): String;
  function VCErrorsStr(AErrors : TVCErrors):String;
  function ChErrorsStr(AErrors : TVChErrors): String;
  function VFErrorsStr(AErrors : TVFErrors) : String;
  function TCOErrorsStr(AErrors : TTOErrors) : String;
  function FTErrorsStr(AErrors : TFTErrors) : String;

implementation

uses
System.Classes,
System.SysUtils,
ThreeDimTypes,
VvarMatrixIF,
Spline,
ChipAbstractInterface,
MilandrRev8.Consts,
System.IniFiles,
Vodopad.Math;


const
C_Protection = 'Защиты';
C_TVMeterFail = 'Время обмена с вольтметром';
C_Registers_write= 'Записи битов';
C_Freq_meas= 'Измерения частоты';
C_Voltage_out_of_range = 'Напряж. вне диапазона';
C_Freq_out_of_limit= 'Допуска по частоте';
C_SSE_out_of_limit=  'Допуска по дисперсии';
C_Temperature_out_of_limit= 'Допуска по температуре';
C_VDD_VC= 'VDD/VC';
C_NominalOutVvar = 'Номинал вне предела VVAR';

{C_Protection ='Protection';
C_TVMeterFail = 'Volts to old';
C_Registers_write='Registers write';
C_Voltage_out_of_range='Voltage out of range';
C_Freq_meas='Freq meas';
C_Freq_out_of_limit='Freq out of limit';
C_SSE_out_of_limit='SSE out of limit';
C_Temperature_out_of_limit='Temperature out of limit';
C_VDD_VC='VDD/VC';
C_NominalOutVvar = 'Nominal freq out of VVAR';}

var

FTErrorStr : array  [TFTError] of String = (C_Protection,
                                              C_TVMeterFail,
                                              C_Freq_meas,
                                              C_Freq_out_of_limit,
                                              C_SSE_out_of_limit,
                                              C_Temperature_out_of_limit,
                                              C_VDD_VC);

FMErrorStr : array  [TFMError] of String = (C_Protection, C_TVMeterFail,
                                              C_Freq_meas,
                                              C_Freq_out_of_limit,
                                              C_SSE_out_of_limit,
                                              C_Temperature_out_of_limit,
                                              C_VDD_VC);
VMErrorStr : array  [TVCError] of String = (C_Protection, C_TVMeterFail,
                                              C_Voltage_out_of_range,
                                              C_Temperature_out_of_limit,
                                              C_VDD_VC);

VChErrorStr : array  [TVChError] of String = (C_Protection, C_TVMeterFail,
                                              C_Voltage_out_of_range,
                                              C_Temperature_out_of_limit,
                                              C_VDD_VC);

VFErrorStr : array  [TVFError] of String = (C_Protection, C_TVMeterFail,
                                              C_Freq_meas,
                                              C_Freq_out_of_limit,
                                              C_SSE_out_of_limit,
                                              C_Temperature_out_of_limit,
                                              C_VDD_VC,
                                              C_NominalOutVvar);

TOErrorStr : array  [TTOError] of String = (C_Protection, C_TVMeterFail,
                                              C_Freq_meas,
                                              C_Freq_out_of_limit,
                                              C_SSE_out_of_limit,
                                              C_Temperature_out_of_limit,
                                              C_VDD_VC);



{TFinalTestObj}
function TFinalTestObj.GetFreqDisp: Double;
begin
  result := fFreqDisp;
end;

procedure TFinalTestObj.SetFreqDisp(const Value: Double);
begin
  fFreqDisp := Value;
end;

function TFinalTestObj.GetItemErrors : TFTErrors;
var
vErrAd : TFTErrorsAdapter;
begin
  vErrAd.i := self.Error;
  result := vErrAd.s;
end;

function TFinalTestObj.GetVC: double;
begin
  result := fVC;
end;

function TFinalTestObj.GetVdd: double;
begin
  result := fVdd;
end;

procedure TFinalTestObj.SetVC(const Value: double);
begin
  fVC := Value;
end;

procedure TFinalTestObj.SetVdd(const Value: double);
begin
  fVdd := Value;
end;

procedure FTFill(const ADB: TEmbSQLDataBase; const ADest : TMeasDataList; AIter: word);
const vSqlText =
      'SELECT TEMPR, FREQ, FREQDISP, ERR_CODE, VDD, VC FROM FREQ_FINAL '+
      'WHERE ITER_NUM = :ITER_NUM ORDER BY IZM_NUM;';
var
vQR : TEmbSQLRequest;
vItem : TFinalTestObj;
begin
  try
    ADest.ClearData;
    ADest.ItemClass := TFinalTestObj;
    try
      vQR.Prepare(ADB.DB, vSqlText);
      vQR.Bind(1, AIter);
      ADest.BeginUpdate;
      try
        while (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW)  do
        begin
          vItem := ADest.NewItem as TFinalTestObj;
          vItem.XValue := vQR.FieldDouble(0);
          vItem.YValue := vQR.FieldDouble(1);
          vItem.FreqDisp := vQR.FieldDouble(2);
          vItem.Error := vQR.FieldInt(3);
          vItem.Vdd := vQR.FieldDouble(4);
          vItem.VC := vQR.FieldDouble(5);
        end;
      finally
        ADest.EndUpdate;
      end;
    except
      ADest.ClearData;
    end;
  finally
    vQR.Close;
  end;
end;

function FTErrorsStr(AErrors : TFTErrors) : String;
var
e : TFTError;
vCount : byte;
begin
  vCount := 0;
  if (AErrors <> []) then
  begin
    for e := Low(e) to High(e) do
    begin
      if (e in AErrors) then
      begin
        if (vCount = 0) then
          Result := FTErrorStr[e]
        else
          Result := Format('%s, %s',[Result, FTErrorStr[e]]);
        inc(vCount);
      end;
    end;
  end else
    Result := 'OK';
end;

{TFreqCurrObj}
function TFreqCurrObj.GetDisp: Double;
begin
  result := fDisp;
end;

function TFreqCurrObj.GetItemErrors: TFMErrors;
var
vErrAd : TFMErrorsAdapter;
begin
  vErrAd.i := self.Error;
  result := vErrAd.s;
end;

procedure TFreqCurrObj.SetDisp(const Value: Double);
begin
  fDisp := Value;
end;

function TFreqCurrObj.GetVC: double;
begin
  result := fVC;
end;

function TFreqCurrObj.GetVdd: double;
begin
  result := fVdd;
end;

procedure TFreqCurrObj.SetVC(const Value: double);
begin
  fVC := Value;
end;

procedure TFreqCurrObj.SetVdd(const Value: double);
begin
  fVdd := Value;
end;

procedure FCFill(const ADB: TEmbSQLDataBase; const ADest : TMeasDataList; AIter, ASolution: word);
const vSqlText =
      'SELECT TEMPR, FREQ, FREQDISP, ERR_CODE, VDD, VC FROM FREQ_CURR '+
      'WHERE ITER_NUM = :ITER_NUM AND SOL_NUM = :SOL_NUM ORDER BY IZM_NUM;';
var
vQR : TEmbSQLRequest;
vItem : TFreqCurrObj;
begin
  try
    ADest.ClearData;
    ADest.ItemClass:= TFreqCurrObj;
    try
      vQR.Prepare(ADB.DB, vSqlText);
      vQR.Bind(1, AIter);
      vQR.Bind(2, ASolution);
      ADest.BeginUpdate;
      try
        while (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW)  do
        begin
          vItem := ADest.NewItem as TFreqCurrObj;
          vItem.XValue := vQR.FieldDouble(0);
          vItem.YValue := vQR.FieldDouble(1);
          vItem.Disp := vQR.FieldDouble(2);
          vItem.Error := vQR.FieldInt(3);
          vItem.Vdd := vQR.FieldDouble(4);
          vItem.VC := vQR.FieldDouble(5);
        end;
      finally
        ADest.EndUpdate;
      end;
    except
      ADest.ClearData;
    end;
  finally
    vQR.Close;
  end;
end;

function FCErrorsStr(AErrors : TFMErrors): String;
var
e : TFMError;
vCount : byte;
begin
  vCount := 0;
  if (AErrors <> []) then
  begin
    for e := Low(e) to High(e) do
    begin
      if (e in AErrors) then
      begin
        if (vCount = 0) then
          result := FMErrorStr[e]
        else
          result := Format('%s, %s',[result, FMErrorStr[e]]);
        inc(vCount);
      end;
    end;
  end else
    result := 'OK';
end;

{TC_OFFObj}
function TTC_OFFObj.GetDisp: Double;
begin
  result := fDisp;
end;

function TTC_OFFObj.GetItemErrors: TTOErrors;
var
vErrAd : TTOErrorsAdapter;
begin
  vErrAd.i := self.Error;
  result := vErrAd.s;
end;

procedure TTC_OFFObj.SetDisp(const Value: Double);
begin
  fDisp := Value;
end;

function TTC_OFFObj.GetVC: double;
begin
  result := fVC;
end;

function TTC_OFFObj.GetVdd: double;
begin
  result := fVdd;
end;

procedure TTC_OFFObj.SetVC(const Value: double);
begin
  fVC := Value;
end;

procedure TTC_OFFObj.SetVdd(const Value: double);
begin
  fVdd := Value;
end;

procedure TCOFill(const ADB: TEmbSQLDataBase; const ADest : TMeasDataList; AIter : word);
const vSqlText =
      'SELECT TEMPR, FREQ, FREQDISP, ERR_CODE, VDD, VC FROM FREQ_TCOFF '+
      'WHERE ITER_NUM = :ITER_NUM ORDER BY IZM_NUM;';
var
vQR : TEmbSQLRequest;
vItem : TTC_OFFObj;
begin
  try
    ADest.ClearData;
    ADest.ItemClass := TTC_OFFObj;
    try
      vQR.Prepare(ADB.DB, vSqlText);
      vQR.Bind(1, AIter);
      ADest.BeginUpdate;
      try
        while (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW)  do
        begin
          vItem := ADest.NewItem as TTC_OFFObj;
          vItem.XValue := vQR.FieldDouble(0);
          vItem.YValue := vQR.FieldDouble(1);
          vItem.Disp := vQR.FieldDouble(2);
          vItem.Error := vQR.FieldInt(3);
          vItem.Vdd := vQR.FieldDouble(4);
          vItem.VC := vQR.FieldDouble(5);
        end;
      finally
        ADest.EndUpdate;
      end;
    except
      ADest.ClearData;
    end;
  finally
    vQR.Close;
  end;
end;

function TCOErrorsStr(AErrors : TTOErrors) : String;
var
e : TTOError;
vCount : byte;
begin
  vCount := 0;
  if (AErrors <> []) then
  begin
    for e := Low(e) to High(e) do
    begin
      if (e in AErrors) then
      begin
        if (vCount = 0) then
          result := TOErrorStr[e]
        else
          result := Format('%s, %s',[result, TOErrorStr[e]]);
        inc(vCount);
      end;
    end;
  end else
    result := 'OK';
end;

{ TVvarMCObj }
function TVvarMCObj.GetItemErrors: TVCErrors;
var
vErrAd : TVCErrorsAdapter;
begin
  vErrAd.i := self.Error;
  result := vErrAd.s;
end;

function TVvarMCObj.GetVC: double;
begin
  result := fVC;
end;

function TVvarMCObj.GetVdd: double;
begin
  result := fVdd;
end;

procedure TVvarMCObj.SetVC(const Value: double);
begin
  fVC := Value;
end;

procedure TVvarMCObj.SetVdd(const Value: double);
begin
  fVdd := Value;
end;

procedure VCFill(const ADB: TEmbSQLDataBase; const ADest : TMeasDataList; AIter, ASolution: word);
const c_SqlText =
      'SELECT TEMPR, VOLTAGE, ERR_CODE, VDD, VC FROM VVAR_CURR '+
      'WHERE ITER_NUM = :ITER_NUM AND SOL_NUM = :SOL_NUM ORDER BY IZM_NUM;';
var
vQR : TEmbSQLRequest;
vItem : TVvarMCObj;
begin
  try
    ADest.ClearData;
    ADest.ItemClass := TVvarMCObj;
    try
      vQR.Prepare(ADB.DB, c_SqlText);
      vQR.Bind(1, AIter);
      vQR.Bind(2, ASolution);
      ADest.BeginUpdate;
      try
        while (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW)  do
        begin
          vItem := ADest.NewItem as TVvarMCObj;
          vItem.XValue := vQR.FieldDouble(0);
          vItem.YValue := vQR.FieldDouble(1);
          vItem.Error := vQR.FieldInt(2);
          vItem.Vdd := vQR.FieldDouble(3);
          vItem.VC := vQR.FieldDouble(4);
        end;
      finally
        ADest.EndUpdate;
      end;
    except
      ADest.ClearData;
    end;
  finally
    vQR.Close;
  end;
end;

function VCErrorsStr(AErrors : TVCErrors):String;
var
e : TVCError;
vCount : byte;
begin
  vCount := 0;
  if (AErrors <> []) then
  begin
    for e := Low(e) to High(e) do
    begin
      if (e in AErrors) then
      begin
        if (vCount = 0) then
          result := VMErrorStr[e]
        else
          result := Format('%s, %s',[result, VMErrorStr[e]]);
        inc(vCount);
      end;
    end
  end else
    result := 'OK';
end;

procedure CharactFill(const ADB: TEmbSQLDataBase; const ATable : TVvarMatrix; AIter, ASolution: word);
const c_SqlMeasDataText =
      'SELECT POINTIDX, TEMPR, VOLTAGE, REGMEM, ERR_CODE, METHOD_TAG, METHOD_VAR, IZM_NUM, VDD, VC  '+
      'FROM VVAR_CHAR WHERE ((ITER_NUM = :ITER_NUM) AND ((SOL_NUM = :SOL_NUM) OR (SOL_NUM = 0))) ORDER BY IZM_NUM';
var
vQR : TEmbSQLRequest;
vColl, vRow: byte;
m_Tag, m_Var : byte;
vTempr : double;
vDepthCell : IDepthCell;
vVvarCell : IVvarCell;
vPointIdx : integer;
vErrorsCount : Word;
begin
  try
    m_Tag := 100;
    m_Var := 100;
    vColl := 0;
    vRow := 0;
    vErrorsCount := 0;
    vPointIdx := -1;
    ATable.Clear;
    try
      {и формируем запрос}
      vQR.Prepare(ADB.DB, c_SqlMeasDataText);
      vQR.Bind(1, AIter);
      vQR.Bind(2, ASolution);
      {Двигаемся по ответу пока отдает Next}
      while (vQR.PrepareNext = SQLEMB_DONE)
      and (vQR.Step = SQLEMB_ROW)  do
      begin
        {Извлекаем значение температуры}
        vTempr := vQR.FieldDouble(1);
        if vQR.FieldInt(0) <> vPointIdx then
        begin
          {По признаку vPointIdx
          детектим новую температурную точку
          и создаем для нее колонку}
          vPointIdx := vQR.FieldInt(0);
          vColl := ATable.AddCol(vTempr);
          vErrorsCount := 0;
        end;

        m_Tag := Byte(vQR.FieldInt(5));
        {детектим новый таг по признаку его наличия в таблице
        и если не нашли, строку с таким тагом, то создаем строку}
        if not ATable.FindRow(m_Tag, vRow) then
          vRow := ATable.AddRow(m_Tag);
        m_Var := Byte(vQR.FieldInt(6));
        if ATable.ByIndex(vColl, vRow, IDepthCell, vDepthCell) then
        begin
          {увеличиваем глубину Z-измерения ячейки}
          vDepthCell.Depth := vDepthCell.Depth + 1;
          if vDepthCell.QueryItem(vDepthCell.Depth - 1, IVvarCell, vVvarCell) then
          begin
            {и ложим данные в последнюю ячейку третьего измерения}
            vVvarCell.mVar := m_Var;
            vVvarCell.ErrCode := vQR.FieldInt(4);
            if vVvarCell.ErrCode <> 0 then
              Inc(vErrorsCount);
            vVvarCell.Voltage := vQR.FieldDouble(2);
            vVvarCell.IzmNum := vQR.FieldInt(7);
            vVvarCell.Vdd := vQR.FieldDouble(8);
            vVvarCell.VC := vQR.FieldDouble(9);
            vQR.SaveBlobFieldToStream(3, vVvarCell.Memory);
            vVvarCell.Memory.Position := 0;
          end;
          vVvarCell := nil;
          vDepthCell.ErrorsCount := vDepthCell.ErrorsCount + vErrorsCount;
        end;
        vDepthCell := nil;
      end;
    except
      ATable.Clear;
    end;
  finally
    vQR.Close;
  end;
end;

function ChErrorsStr(AErrors : TVChErrors): String;
var
e : TVChError;
vCount : byte;
begin
  vCount := 0;
  if (AErrors <> []) then
  begin
    for e := Low(e) to High(e) do
    begin
      if (e in AErrors) then
      begin
        if (vCount = 0) then
          result := VChErrorStr[e]
        else
          result := Format('%s, %s',[result, VChErrorStr[e]]);
        inc(vCount);
      end;
    end
  end else
    result := 'OK';
end;




function TFCitem.GetDisp: double;
begin
  result := fFreqDispPpm;
end;

procedure TFCitem.SetDisp(AValue: double);
begin
  fFreqDispPpm := AValue;
end;

function TFCitem.GetItemErrors: TVFErrors;
var
vTmp : TVFErrorsAdapter;
begin
  vTmp.i := self.Error;
  result := vTmp.s;
end;

function TFCitem.GetRegisters: TMilandrRev8Registers;
begin
  result := fRegisters;
end;

function TFCitem.GetVC: double;
begin
  result := fVC;
end;

function TFCitem.GetVdd: double;
begin
  result := fVdd;
end;

procedure TFCitem.SetFcError(Value: TVFErrors);
var
vTmp : TVFErrorsAdapter;
begin
  vTmp.i := 0;
  vTmp.s := Value;
  self.Error := vTmp.i;
end;

procedure TFCitem.SetVC(AValue: double);
begin
  fVC := AValue;
end;

procedure TFCitem.SetVdd(AValue: double);
begin
  fVdd := AValue;
end;

procedure TFCitem.ErrorInclude(AError: TVFError);
var
vTmp : TVFErrorsAdapter;
begin
  vTmp.i := self.Error;
  include(vTmp.s, AError);
  self.Error := vTmp.i;
end;

constructor TFCitem.Create(ADataList: TMeasDataList);
begin
  inherited;
  fRegisters := TMilandrRev8Registers.Create(nil);
end;

destructor TFCitem.Destroy;
begin
  FreeAndNil(fRegisters);
  inherited;
end;

procedure TFCitem.ErrorExclude(AError: TVFError);
var
vTmp : TVFErrorsAdapter;
begin
  vTmp.i := self.Error;
  Exclude(vTmp.s, AError);
  self.Error := vTmp.i;
end;



{ TVvarFCObj }

function TVvarFCObj.GetItemErrors: TVFErrors;
var
vErrAd : TVFErrorsAdapter;
begin
  vErrAd.i := self.Error;
  result := vErrAd.s;
end;

procedure TVvarFCObj.ErrorInclude(AError: TVFError);
var
vTmp : TVFErrorsAdapter;
begin
  vTmp.i := self.Error;
  include(vTmp.s, AError);
  self.Error := vTmp.i;
end;


class function TFCPoint.CalcErrors(ADataList: TMeasDataList): word;
var
vIdx : Integer;
vItem : TFCPoint;
begin
  result := 0;
  for vIdx := 0 to ADataList.Count - 1 do
  begin
    if ADataList.QueryItem(vIdx, TFCPoint, vItem) then
      inc(result, vItem.FreqCurve.ErrorsCount);
  end;
end;

class function TFCPoint.CalcTotalCount(ADataList: TMeasDataList): word;
var
vIdx : Integer;
vItem : TFCPoint;
begin
  result := 0;
  for vIdx := 0 to ADataList.Count - 1 do
  begin
    if ADataList.QueryItem(vIdx, TFCPoint, vItem) then
      inc(result, vItem.FreqCurve.ItemsCount);
  end;
end;

constructor TFCPoint.Create(ADataList: TMeasDataList);
begin
  inherited Create(ADataList);
  fFreqCurve := TMeasDataList.Create(st_linear);
  fFreqCurve.ItemClass := TFCitem;
end;

destructor TFCPoint.Destroy;
begin
  FreeAndNil(fFreqCurve);
  inherited Destroy;
end;

class procedure TFCPoint.EndUpdate(ADataList: TMeasDataList);
begin
end;

function TFCPoint.ErrorsExists(AErrors: TVFErrors): Boolean;
var
vIdx : Integer;
vItem : TFCitem;
vError : TVFError;
vItemErrors : TVFErrors;
begin
  result := fFreqCurve.Count<1;
  if not Result then
    Exit;
  for vIdx := 0 to fFreqCurve.Count - 1 do
  begin
    vItem := nil;
    if fFreqCurve.QueryItem(vIdx, TFCitem, vItem)
    and (vItem.Error > 0) then
    begin
      vItemErrors := vItem.ItemErrors;
      for vError := Low(vError) to High(vError) do
      begin
        Result := (vError in AErrors) and (vError in vItemErrors);
        if result then
          Break;
      end;
    end;
    if result then
      Break;
  end;
end;

function TFCPoint.FreqCurve: TMeasDataList;
begin
  result := fFreqCurve;
end;

procedure VFCFill(const ADB: TEmbSQLDataBase; const ADBObj : TPositionDBExtention;
                        const Iter : TMilIterationResults; const Dest: TFCMeasResults);
const vSqlText =
      'SELECT  IZM_NUM, POINTIDX, TEMPR, '+
      'VDD, VC, VOLTAGE, FREQ, FREQDISP, ERR_CODE, REGMEM FROM VVAR_FC '+
      'WHERE ITER_NUM = :ITER_NUM ORDER BY IZM_NUM';

C_Vvar_Facking_Error_Value = 0.2;
var
vQR : TEmbSQLRequest;
vVvarCurve : TMeasDataList;
vVvarCurveItem : TMeasDataItem;
vFCItem : TFCItem;
vFCPoint : TFCPoint;
vX : double;
vPointIdx : Integer;
vVDD_VCErrors : word;
vMS : TMemoryStream;
  procedure FinalizeCurves;
  var
  vFCVarY : double;
  vFCNewForceItem : TVvarFCObj;
  vFCNominalOut : Boolean;
  begin
    if Assigned(vFCPoint) then
      vFCPoint.FreqCurve.EndUpdate;
    vVvarCurve.EndUpdate;
    if (vVvarCurve.Splines.Count > 0)
    and (vVvarCurve.Splines[0].Count >= 3)  then
    begin
      vFCNominalOut := False;
      if (ADBObj.Nominal < vVvarCurve.Splines[0].FirstX) then
      begin
        vFCVarY := vVvarCurve.Splines[0].FirstY - C_Vvar_Facking_Error_Value;
        vFCNominalOut := True;
      end else
      if (ADBObj.Nominal > vVvarCurve.Splines[0].LastX) then
      begin
        vFCVarY := vVvarCurve.Splines[0].LastY + C_Vvar_Facking_Error_Value;
        vFCNominalOut := True;
      end else
        vFCVarY := vVvarCurve.YX(ADBObj.Nominal);

      vFCNewForceItem := Dest.Force.NewItem as TVvarFCObj;
      vFCNewForceItem.XValue := vX;
      vFCNewForceItem.YValue := vFCVarY;
      if vFCNominalOut then
        vFCNewForceItem.ErrorInclude(fceNominalOutVvar);
      if (vVDD_VCErrors > 0) then
         vFCNewForceItem.ErrorInclude(fceVDD_VC);
    end;
  end;
begin
  vVvarCurve := TMeasDataList.Create;
  vMS := TMemoryStream.Create;
  try
    Dest.Clear;
    vPointIdx := -1;
    vFCPoint := nil;
    try
      vQR.Prepare(ADB.DB, vSqlText);
      vQR.Bind(1, Iter.Num);
      Dest.BeginUpdate;
      try
        while (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW)  do
        begin
          if vPointIdx <> vQR.FieldInt(1) then
          begin
            if (vPointIdx > -1) then
              FinalizeCurves;
            vPointIdx := vQR.FieldInt(1);
            vFCPoint := Dest.Curves.NewItem as TFCPoint;
            vX := vQR.FieldDouble(2);
            vFCPoint.XValue := vX;
            vVvarCurve.BeginUpdate;
            vVvarCurve.ClearData;
            vVDD_VCErrors := 0;
          end;
          if Assigned(vFCPoint) then
          begin
            vFCItem := vFCPoint.FreqCurve.NewItem as TFCItem;
            vFCItem.XValue := vQR.FieldDouble(5);
            vFCItem.YValue := vQR.FieldDouble(6);
            vFCItem.DispPpm := vQR.FieldDouble(7);
            vFCItem.Vdd := vQR.FieldDouble(3);
            vFCItem.VC :=  vQR.FieldDouble(4);
            vFCItem.Error := vQR.FieldInt(8);
            vMS.Clear;
            vQR.SaveBlobFieldToStream(9, vMS);
            vMS.Position := 0;
            vFCItem.Registers.LoadFromStream(vMS);
            if (fceVDD_VC in vFCItem.ItemErrors) then
              inc(vVDD_VCErrors);
          end;
          vVvarCurveItem := vVvarCurve.NewItem;
          vVvarCurveItem.XValue := vQR.FieldDouble(6);
          vVvarCurveItem.YValue := vQR.FieldDouble(5);
        end;
      finally
        FinalizeCurves;
        Dest.EndUpdate;
      end;
    except
      Dest.Clear;
    end;
  finally
    vQR.Close;
    FreeAndNil(vVvarCurve);
    FreeAndNil(vMS);
  end;
end;


function VFErrorsStr(AErrors : TVFErrors) : String;
var
e : TVFError;
vCount : byte;
begin
  vCount := 0;
  if (AErrors <> []) then
  begin
    for e := Low(e) to High(e) do
    begin
      if (e in AErrors) then
      begin
        if (vCount = 0) then
          Result := VFErrorStr[e]
        else
          Result := Format('%s, %s',[Result, VFErrorStr[e]]);
        inc(vCount);
      end;
    end
  end else
    Result := 'OK';
end;

{ TFCMeasResults }

procedure TFCMeasResults.BeginUpdate;
begin
  fCurves.BeginUpdate;
  fForce.BeginUpdate;
end;

procedure TFCMeasResults.Clear;
begin
  fCurves.ClearData;
  fForce.ClearData;
end;

constructor TFCMeasResults.Create;
begin
  inherited;
  fCurves := TMeasDataList.Create;
  fForce := TMeasDataList.Create;
  Curves.ItemClass := TFCPoint;
  Force.ItemClass := TVvarFCObj;
end;

destructor TFCMeasResults.Destroy;
begin
  fCurves.Clear;
  fForce.Clear;
  FreeAndNil(fCurves);
  FreeAndNil(fForce);
  inherited Destroy;
end;

procedure TFCMeasResults.EndUpdate;
begin
  fCurves.EndUpdate;
  fForce.EndUpdate;
end;


{ TMilResultsDBObj }

procedure TMilResultsDBObj.AfterCreate;
begin
  inherited;
  fIterations := TObjectList<TMilIterationResults>.Create;
end;

procedure TMilResultsDBObj.BeforeDestroy;
begin
  fIterations.Clear;
  FreeAndNil(fIterations);
  inherited;
end;

class function TMilResultsDBObj.DefaultIterationClass: TIterationClass;
begin
  Result := TMilIterationResults;
end;

procedure TMilResultsDBObj.Load(const AFileName : string; ACalcResults : Boolean);
var
vDB: TEmbSQLDataBase;
vPosData : TMemIniFile;
vFormatSettings: TFormatSettings;
vIterResults : TMilIterationResults;
i : Integer;
begin
  fIterations.Clear;
  vPosData := TMemIniFile.Create('');
  vFormatSettings := TFormatSettings.Create(1033);
  try
    vDB := CreateConnection(AFileName);
    try
      if ReadPosData(vDB)
      and ReadPosData(vDB, vPosData)
      and ReadPosData(vPosData, vFormatSettings)
      and IsEqualGUID(ChipGUID, MilandrRev8.Consts.C_GUID)
      and (ItersCount > 0) then
      begin
        i := 0;
        while i < ItersCount do
        begin
          vIterResults := TMilIterationResults.Create(self);
          if vIterResults.Read(vDB, i+1, vFormatSettings)
          and (isSucc in vIterResults.State) then
          begin
            fIterations.Add(vIterResults);
            vIterResults.Load(vDB, ACalcResults);
          end else
            FreeAndNil(vIterResults);
          Inc(i);
        end;
      end;
    finally
      if vDB.TransactionActive then
        vDB.RollBack;
      vDB.DBClose;
      FreeAndNil(vDB);
    end;
  finally
    FreeAndNil(vPosData);
  end;
end;

{ TCalcCurves }



class procedure TCalcErrorObj.EndUpdate(ADataList: TMeasDataList);
var
vCE : TCalcErrorObj;
vMESpl, vFESpl: TSpline;
vIdx : Integer;
vOkCount : word;
begin
  inherited EndUpdate(ADataList);
  vOkCount := ADataList.Count;
  if (vOkCount < 3) then
    exit;
  vMESpl := ADataList.Splines.NewItem(vOkCount);
  vFESpl := ADataList.Splines.NewItem(vOkCount);
  for vIdx := 0 to ADataList.Count - 1 do
  begin
    if ADataList.QueryItem(vIdx, TCalcErrorObj, vCE) then
    begin
      {if (vCE.Error = 0) then
      begin }
      vMESpl.AddXY(vCE.XValue, vCE.MVolts);
      vFESpl.AddXY(vCE.XValue, vCE.Freq);
      //end;
    end;
  end;
end;

function TCalcErrorObj.FreqX(AX: Double): Double;
begin
  result := 0;
  if DataList.Splines.Count < 3 then
    exit;
  Result := DataList.Splines[2].Y(AX);
end;

function TCalcErrorObj.MVoltsX(AX: Double): Double;
begin
  result := 0;
  if DataList.Splines.Count < 2 then
    exit;
  Result := DataList.Splines[1].Y(AX);
end;

function TCalcErrorObj.GetFreq: Double;
begin
  result := fFreq;
end;

function TCalcErrorObj.GetMVolts: Double;
begin
  result := fMvError;
end;

procedure TCalcErrorObj.SetFreq(AValue: Double);
begin
  fFreq := AValue;
end;

procedure TCalcErrorObj.SetMVolts(AValue: Double);
begin
  fMvError := AValue;
end;


function CalcDeltaReg(const AInitVect, ADestVect :TMilandrRev8Registers; const RegWeighs : TRegistersDouble):double; overload;
var
i : byte;
vdRegI, RegWSumm : double;
begin
  Result := 0;
  RegWSumm := 0;
  if RegWeighs.Count < 1 then
    Exit;
  for i := 0 to RegWeighs.Count -1 do
  begin
    vdRegI := abs((AInitVect.BitValue[RegWeighs[i].BitIndex] - ADestVect.BitValue[RegWeighs[i].BitIndex]) /
     (C_ValsMax[RegWeighs[i].BitIndex] - C_ValsMin[RegWeighs[i].BitIndex]));
    RegWSumm := RegWSumm + RegWeighs[i].Value * vdRegI;
  end;
  result :=  RegWSumm;
end;

procedure TCalcCurves.CalcCurves(const ARegisters: TMilandrRev8Registers);
var
vPointIdx :Word;
vCurveIdx : integer;
vTemprI,
vFreqCalcX,
vVarCalcX,
vVarForceCalcX,
vCurr_ppm_Error, vFreq25 : Double;
vFC : TFCPoint;
vVVF : TVvarFCObj;
vCO  : TCalcErrorObj;
begin
  fRootObj.CharactTable.CalcVvar(fRootObj.Registers, ARegisters, fRootObj.Vvar, Self.Vvar);
  if (Self.Vvar.Count < 1)
    or(IterationMil.FCResults.Curves.Count < 1)
    or (Self.Vvar.Splines.Count < 1)
    or (Self.Vvar.Splines[0].Count < 3) then
    Exit;
  fFreq.ItemClass := TCalcErrorObj;
  fFreq.BeginUpdate;
  try
    fFreq.ClearData;
    if IterationMil.MeasPoints.Count < 1 then
      Exit;
    for vPointIdx :=0 to IterationMil.MeasPoints.Count -1 do
    begin
      vTemprI := IterationMil.MeasPoints[vPointIdx].PointValue;
      if not IterationMil.FCResults.Curves.XExists(vTemprI, 0.5, vCurveIdx) then
        Continue;
      vFC := IterationMil.FCResults.Curves[vCurveIdx] as TFCPoint;
      if (vFC.FreqCurve.Splines.Count < 1)
      or (vFC.FreqCurve.Splines[0].Count < 3) then
       Continue;
      vVarCalcX := Self.Vvar.YX(vTemprI);
      if (vFC.FreqCurve.Count >=3)
      and (vFC.FreqCurve.Splines.Count > 0) then
      begin
        vFreqCalcX := vFC.FreqCurve.YX(vVarCalcX);
        vCurr_ppm_Error := ppm(vFreqCalcX, IterationMil.DBExt.Nominal);
        vCO := fFreq.NewItem as TCalcErrorObj;
        vCO.XValue := vTemprI;
        vCO.Freq := vFreqCalcX;
        vCO.YValue := vCurr_ppm_Error;
        if IterationMil.FCResults.Force.QueryItem(0, TVvarFCObj, vVVF) then
        begin
          vVarForceCalcX := vVVF.YX(vTemprI);
          vCO.MVolts := (vVarCalcX - vVarForceCalcX) * 1000;
        end else
          vCO.MVolts := 0;
      end;
    end;
  finally
    Freq.EndUpdate;
    if (Freq.Count >=3)
    and (Freq.Splines.Count > 0) then
    begin
      Self.Results.dF25 := Freq.YX(25);
      vFreq25 := (Freq[0] as TCalcErrorObj).FreqX(25);
    end else
      vFreq25 := 1;
    fdReg := CalcDeltaReg(fRootObj.Registers, ARegisters, IterationMil.Options.CalcOptions.RegistersWeighs);
  end;


  Results.dF := 0;
  vCurveIdx := 0;
  case IterationMil.Options.CalcOptions.CalcMethod  of
    cmGeneral:
    begin
      while vCurveIdx < Freq.ItemsCount do
      begin
        if Freq.QueryItem(vCurveIdx, TCalcErrorObj, vCO) then
        begin
          vTemprI := vCO.XValue;
          if IterationMil.MeasPoints.Find(vTemprI, 0.7, vPointIdx)
          and  IterationMil.MeasPoints[vPointIdx].WorkPoint then
          begin
            vCurr_ppm_Error := abs(vCO.YValue);
            if vCurr_ppm_Error > Results.dF then
              Results.dF := vCurr_ppm_Error;
          end;
        end;
        inc(vCurveIdx);
      end;
      fPrior := Results.dF + fdReg;
    end;
    cm25:
    begin
      while vCurveIdx < Freq.ItemsCount do
      begin
        if Freq.QueryItem(vCurveIdx, TCalcErrorObj, vCO) then
        begin
          vTemprI := vCO.XValue;
          if IterationMil.MeasPoints.Find(vTemprI, 0.7, vPointIdx)
          and  IterationMil.MeasPoints[vPointIdx].WorkPoint then
          begin
            vCurr_ppm_Error := Absppm(vCO.Freq, vFreq25);
            if vCurr_ppm_Error > Results.dF then
              Results.dF := vCurr_ppm_Error;
          end;
        end;
        inc(vCurveIdx);
      end;
      fPrior := Results.dF + fdReg + IterationMil.Options.CalcOptions.cm25Coeff * Absppm(vFreq25, IterationMil.DBExt.Nominal);
    end;
  end;
end;

procedure TCalcCurves.CalcCurves;
begin
  CalcCurves(Registers);
end;

constructor TCalcCurves.Create(const AIteration: TMilIterationBase; const ARootIdx : Byte);
begin
  inherited Create(AIteration, ARootIdx);
  fVvar := TMeasDataList.Create;
  fFreq := TMeasDataList.Create;
  fIterationMil := TMilIterationResults(AIteration);
  fRootObj := fIterationMil.MeasSolutions[RootIdx];
end;

destructor TCalcCurves.Destroy;
begin
  fFreq.Clear;
  fVvar.Clear;
  FreeAndNil(fFreq);
  FreeAndNil(fVvar);
  inherited;
end;
{ TMeasureSolutionCurves }


constructor TMeasureSolutionCurves<T>.Create(const AIteration: TMilIterationBase);
begin
  inherited Create(AIteration);
  fIterationMas := TMilIterationResults(AIteration);
  fVvar := TMeasDataList.Create;
  fFreq := TMeasDataList.Create;
  fVvarMatrix := TVvarMatrix.Create;
  fCharactTable := TCTMilRev8.Create;
end;

destructor TMeasureSolutionCurves<T>.Destroy;
begin
  fCharactTable.Clear;
  fVvarMatrix.Clear;
  fFreq.ClearData;
  fVvar.ClearData;
  FreeAndNil(fCharactTable);
  FreeAndNil(fVvarMatrix);
  FreeAndNil(fFreq);
  FreeAndNil(fVvar);
  inherited Destroy;
end;

procedure TMeasureSolutionCurves<T>.Load(const ADB: TEmbSQLDataBase; ASolIdx : Word);
var
vFreqCurrObj : TFreqCurrObj;
vFreq25, vTmp, vdF : Double;
vIdx, vPointIdx : Word;
begin
  Results.dF := 0;
  Results.dF25 := 0;
  FCFill(ADB, fFreq, Iteration.Num, ASolIdx+1);
  VCFill(ADB, fVvar, Iteration.Num, ASolIdx+1);
  CharactFill(ADB, fVvarMatrix, Iteration.Num, ASolIdx+1);
  fCharactTable.Fill(fVvarMatrix);
  if fFreq.ErrorsCount >= fFreq.Count then
  begin
    Results.dF := 1E9;
    Results.dF25 := 1E9;
    Exit;
  end;
  if fFreq.QueryItem(0, TFreqCurrObj, vFreqCurrObj) then
  begin
    vFreq25 := vFreqCurrObj.YX(25);
    Results.dF25 := ppm(vFreq25, Iteration.DBExt.Nominal);
  end else
    Results.dF25 := 1E6;
  Results.dF := -1;
  vFreqCurrObj := nil;
  vIdx := 0;
  while vIdx < fFreq.Count do
  begin
    if fFreq.QueryItem(vIdx, TFreqCurrObj, vFreqCurrObj) then
    begin
      if vFreqCurrObj.Error = 0 then
      begin
        vdF := Absppm(vFreqCurrObj.YValue, Iteration.DBExt.Nominal);
        case IterationMas.Options.CalcOptions.CalcMethod of
          cmGeneral: vdF := Absppm(vFreqCurrObj.YValue, Iteration.DBExt.Nominal);
          cm25 : vdF := Absppm(vFreqCurrObj.YValue, vFreq25);
        end;
        vTmp := vFreqCurrObj.XValue;
        if IterationMas.MeasPoints.Find(vTmp, 0.5, vPointIdx)
        and IterationMas.MeasPoints[vPointIdx].WorkPoint
        and (vdF > Results.dF) then
          Results.dF := vdF;
      end;
    end;
    Inc(vIdx);
  end;
end;

{ TMilIterationResults }

constructor TMilIterationResults.Create(const ADBExt: TPositionDBExtention);
begin
  inherited Create(ADBExt);
  fFCResults := TFCMeasResults.Create;
  fTC_OFF := TMeasDataList.Create;
  fFreqFinal := TMeasDataList.Create;
end;

destructor TMilIterationResults.Destroy;
begin
  fFCResults.Clear;
  fTC_OFF.ClearData;
  fFreqFinal.ClearData;
  FreeAndNil(fFreqFinal);
  FreeAndNil(fTC_OFF);
  FreeAndNil(fFCResults);
  inherited Destroy;
end;

procedure TMilIterationResults.Load(const ADB: TEmbSQLDataBase; ACalcResults : Boolean);
var
i, j : Word;
vMeasObj : TMeasDataItem;
vTmp, vF, vFNominal : Double;
begin
  fTC_OFFMaxDev := 0;
  if (rpVvarFC in Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s) then
    VFCFill(ADB, DBExt, Self, fFCResults);

  if ([rpVvarCurrMeas, rpFreqCurr] <= Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s)
  and (MeasSolutions.Count > 0) then
  begin
    i := 0;
    while i < self.MeasSolutions.Count do
    begin
      MeasSolutions[i].Load(ADB, i);
      if ACalcResults then
      begin
        j := 0;
        while j < MeasSolutions[i].CalcSolutions.Count do
        begin
          MeasSolutions[i].CalcSolutions[j].CalcCurves;
          inc(j);
        end;
      end;
      Inc(i);
    end;
    if ACalcResults then
      TCalcCurves(CalcResults).CalcCurves;
  end;
  if (rpFreqZero in Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s) then
  begin
    TCOFill(ADB, fTC_OFF, Self.Num);
    if fTC_OFF.ErrorsCount < fTC_OFF.Count then
    begin
      vFNominal := fTC_OFF.YX(25); //Self.DBExt.Nominal
      i := 0;
      while i < fTC_OFF.Count do
      begin
        if fTC_OFF.QueryItem(i, TMeasDataItem, vMeasObj) then
        begin
          if (vMeasObj.Error = 0) then
          begin
            vTmp := vMeasObj.XValue;
            vF := ppm(vMeasObj.YValue, vFNominal);
            if MeasPoints.Find(vTmp, 0.5, j)
            and (MeasPoints[j].WorkPoint)
            and (abs(vF) > fTC_OFFMaxDev) then
              fTC_OFFMaxDev := abs(vF);
          end;
        end;
        Inc(i);
      end;
    end;
  end;
  if (rpFreqFinal in Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s) then
    FTFill(ADB, fFreqFinal, Self.Num);
  if ACalcResults then
    RecalcMeasureResults;
end;

procedure TMilIterationResults.RecalcMeasureResults;
var
i, j : Word;
vFOCurr : TFinalTestObj;
vTmp, vF, vFreq25 : Double;
vCurrIterIdx : Integer;
vPrevIter : TMilIterationResults;
begin
  Results.dF := 1E6;
  Results.dF25 := 1E6;
  Results.Iter_dF25 := 0;
  Results.Iter_dF := 0;
  if (([rpVvarCurrMeas, rpFreqCurr] <= Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s)
  and (MeasSolutions.Count > 0)
  and (isSucc in State)) then
  begin
    i := 0;
    while i < self.MeasSolutions.Count do
    begin
      if (MeasSolutions[i].Results.dF < Results.dF)
      {and (Abs(MeasSolutions[i].Results.dF25) < Results.dF25)} then
      begin
        Results.dF := MeasSolutions[i].Results.dF;
        Results.dF25 := MeasSolutions[i].Results.dF25;
      end;
      Inc(i);
    end;
  end;
  if (((rpFreqZero in Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s)
  or (rpFreqFinal in Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s))
  and (isSucc in State)) then
  begin
    vPrevIter := nil;
    if (DBExt is TMilResultsDBObj)
    and (TMilResultsDBObj(DBExt).Iterations.Count > 1) then
    begin
      vCurrIterIdx := TMilResultsDBObj(DBExt).Iterations.IndexOf(self);
      if (vCurrIterIdx <> -1)
      and (vCurrIterIdx > 0) then
        vPrevIter := TMilResultsDBObj(DBExt).Iterations[vCurrIterIdx-1];
    end;

    if  (rpFreqZero in Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s)
    and Assigned(vPrevIter)
    and (rpFreqZero in vPrevIter.Options.ControlOptions.InRangeOptions[vPrevIter.Options.ControlOptions.MeasureMode].s) then
      CalcIterStab(vPrevIter.TC_OFF, TC_OFF);

    if (rpFreqFinal in Options.ControlOptions.InRangeOptions[Options.ControlOptions.MeasureMode].s) then
    begin
      if (fFreqFinal.ErrorsCount < fFreqFinal.Count) then
      begin
        Results.dF := 0;
        Results.dF25 := 0;
        if fFreqFinal.QueryItem(0, TFinalTestObj, vFOCurr) then
        begin
          vFreq25 := vFOCurr.YX(25);
          Results.dF25 := ppm(vFreq25, DBExt.Nominal);
        end else
          vFreq25 := 1;
        i := 0;
        while i < fFreqFinal.Count do
        begin
          if fFreqFinal.QueryItem(i, TFinalTestObj, vFOCurr) then
          begin
            vTmp := vFOCurr.XValue;
            vF := Absppm(vFOCurr.YValue, DBExt.Nominal);
            case Options.CalcOptions.CalcMethod of
              cmGeneral: vF := Absppm(vFOCurr.YValue, DBExt.Nominal);
              cm25 : vF := Absppm(vFOCurr.YValue, vFreq25);
            end;
            if MeasPoints.Find(vTmp, 0.5, j)
            and MeasPoints[j].WorkPoint
            and (vF > Results.dF) then
                Results.dF := vF;
          end;
          Inc(i);
        end;
      end;
      if Assigned(vPrevIter)
      and (rpFreqFinal in vPrevIter.Options.ControlOptions.InRangeOptions[vPrevIter.Options.ControlOptions.MeasureMode].s) then
        CalcIterStab(vPrevIter.FreqFinal, FreqFinal);
    end;
  end;
end;

procedure TMilIterationResults.CalcIterStab(ACurvePrev, ACurveCurr : TMeasDataList);
var
vCOCurr, vCOPrev : TMeasDataItem;
vsIdx, vCurrIdx, vPrevIdx: integer;
vF25Curr,
vF25Prev,
vDf25, vFPrevShift, vdFAbs : double;
begin

  if ((not ACurveCurr.QueryItem(0, TMeasDataItem, vCOCurr))
  or (not ACurvePrev.QueryItem(0, TMeasDataItem, vCOPrev))) then
    exit;

  vF25Curr := vCOCurr.YX(25);
  vF25Prev := vCOPrev.YX(25);
  vCOCurr := nil;
  vCOPrev := nil;
  vDf25 := vF25Curr - vF25Prev;

  Results.Iter_dF25 := Ppm(vF25Curr, vF25Prev);
  Results.Iter_dF := 0;

  for vsIdx := 0 to MeasPoints.Count - 1 do
  begin
    if ACurvePrev.XExists(MeasPoints[vsIdx].PointValue, 0.5, vPrevIdx)
    and ACurveCurr.XExists(MeasPoints[vsIdx].PointValue, 0.5, vCurrIdx)
    and ACurvePrev.QueryItem(vPrevIdx, TMeasDataItem, vCOPrev)
    and ACurveCurr.QueryItem(vCurrIdx, TMeasDataItem, vCOCurr)
    and (vCOPrev.Error = 0)
    and (vCOCurr.Error = 0)  then
    begin
      vFPrevShift := vCOPrev.YValue + vDf25;
      vdFAbs := absppm(vCOCurr.YValue, vFPrevShift);
      if (vdFAbs > Results.Iter_dF) then
        Results.Iter_dF := vdFAbs;
    end;
  end;
end;

function TMilIterationResults.TotalErrorPercens: double;
var
vMeasIdx,
vTotalMeas,
vTotalError : word;
e : TMeasErrorDescript;
vItem : pMedItem;
begin
  vMeasIdx := 0;
  vTotalMeas := 0;
  vTotalError := 0;
  while vMeasIdx < MeasSolutions.Count do
  begin
    for e := low(e) to high(e) do
      if MeasSolutions.Items[vMeasIdx].Errors.Find(e, vItem) then
      begin
        inc(vTotalMeas, vItem.TotalCount);
        inc(vTotalError, vItem.ErrCount);
      end;
    inc(vMeasIdx);
  end;
  for e := low(e) to high(e) do
  begin
    if Self.Errors.Find(e, vItem) then
    begin
      inc(vTotalMeas, vItem.TotalCount);
      inc(vTotalError, vItem.ErrCount);
    end;
  end;
  if vTotalMeas > 0 then
    result := vTotalError*100/vTotalMeas
  else
    result := 0;
end;

end.
