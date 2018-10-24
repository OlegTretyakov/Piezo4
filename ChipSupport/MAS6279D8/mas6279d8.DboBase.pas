unit mas6279d8.DboBase;

interface

uses
  System.Classes, Stp8ProcessDBOptions,
  System.SysUtils, System.IniFiles,
  System.Generics.Collections, Stp8PositionDBExtention,
  MAS6279D8IMS, System.Generics.Defaults;
 type

    TStartTestProcOption = (cpoWRTest, cpoFreqTest);
    TPreparePointProcOption = (ncInfTrim1, ncCDAC, ncInfTrim2);
    TInRangeProcOption = (rpVvarCharactMeas, rpVvarFC, rpVvarCurrMeas,
               rpFreqCurr, rpFreqZero, rpFreqFinal);
    TAfterCompensProcOption = (arCalc, arCDACbp, arProg);

    TStartTestOptionsSet = set of TStartTestProcOption;
    TPreparePointProcOptionSet = set of TPreparePointProcOption;
    TInRangeProcOptionSet = set of TInRangeProcOption;
    TAfterCompensProcOptionSet = set of TAfterCompensProcOption;
    TStartTestOptions = record
                  case integer of
                  0: (s: TStartTestOptionsSet);
                  1: (i: SmallInt);
                  end;
    TPreparePointOptions = record
                  case integer of
                  0: (s: TPreparePointProcOptionSet);
                  1: (i: SmallInt);
                  end;
    TInRangeOptions = record
                  case integer of
                  0: (s: TInRangeProcOptionSet);
                  1: (i: SmallInt);
                  end;
    TAfterCompensOptions = record
                  case integer of
                  0: (s: TAfterCompensProcOptionSet);
                  1: (i: SmallInt);
                  end;


    TMeasureMode = (pmMeas, pmFinalTest);
    TCalcMethod = (cmGeneral, cm25{, cmPeak});
    TInPointOption = record
     PointID,
     PatternID : Integer;
     PointValue : Double;
     PatternExists,
     MeasInf,
     WorkPoint : Boolean;
     class operator Equal(const A, B : TInPointOption) : Boolean;
     procedure Assign(ASource : TInPointOption);
    end;


    TmasProcessDBOptions = class(TStp8ProcessDBOptions)
     protected
      procedure Read(const ASource : TMemIniFile; const AFormatSettings: TFormatSettings); override;
      procedure Write(const ADest : TMemIniFile; const AFormatSettings: TFormatSettings); override;
     public
      ControlOptions : record
        InfPoint : Double;
        AllowChangeCDACNominal : boolean;
        StartRegistersPatternID,
        AdditionalMeasurePatternID : Integer;
        MeasureMode : TMeasureMode;
        StartTestOptions : TStartTestOptions;
        PreparePointOptions : TPreparePointOptions;
        InRangeOptions :array [TMeasureMode] of TInRangeOptions;
        PowerTurnOffOnInFinalTest : boolean;
        InPointOptions : TList<TInPointOption>;
        AfterCompensOptions : TAfterCompensOptions;
      end;
      ErrorLimits :  record
        ppmMaxDeviation, ppmMaxDispersion : double;
        {допуски на измеренные напряжения
        VDD и VC}
        VDDTolerance,
        VCTolerance : Double;
        {ограничения   напряжения vvar
        именно они д.б использованы.
        копируются из чипа в момент создания или редактирования МК и доступны для редактирования}
        VvarMin, VvarMax : double;
      end;
      CalcOptions : record
        RegWeighPatternID : Integer;
        {метод расчета типа "обычный", "25°С", или "peak-to-peak"}
        CalcMethod : TCalcMethod;

        {максимально количество решений, генерируемое одним измерением}
        MaxMeasuresCalcCount,
        {максимальное количесво решений, генерируемое итерацией
         для передачи на следующую итерацию}
        MaxNextIterCalcCount,
        {количество итераций перед проверкой на возможность програмирования}
        ItersBeforeProgCount : byte;
        {Коэффициент отношения Номинал/ТЧХ при расчете оценки для метода "25°С"}
        cm25Coeff : double;
      end;

      constructor Create; override;
      destructor Destroy;override;
      procedure Assign(const ASource : TStp8ProcessDBOptions); override;
      function Equals(const ASource : TStp8ProcessDBOptions):Boolean; override;
    end;
    TRegisterValueDouble = record
      BitIndex : Byte;
      Value : double;
    end;
    TRegistersDouble = class(TList<TRegisterValueDouble>)
     public
      procedure Assign(const ASource : TRegistersDouble);
      function IndexOfBit(ABitIndex : byte): Integer;
      procedure Read(const ASource : TMemIniFile; const AFormatSettings: TFormatSettings); overload;
      procedure Read(const ASource : TMemIniFile; const ASection : string; const AFormatSettings: TFormatSettings); overload;
      procedure Write(const ADest : TMemIniFile; const AFormatSettings: TFormatSettings);
    end;
    TIterOptions = class(TObject)
    public
      ControlOptions : record
        MeasureMode : TMeasureMode;
        StartTestOptions : TStartTestOptions;
        PreparePointOptions : TPreparePointOptions;
        InRangeOptions :array [TMeasureMode] of TInRangeOptions;
        AfterCompensOptions : TAfterCompensOptions;
      end;
      ErrorLimits :  record
         {ограничения   напряжения vvar
         именно они д.б использованы.
         доступны для редактирования в МК.
         копируются из МК в итерацию в момент создания итерации и по завершению текущей}
        VvarMin, VvarMax : double;
      end;
      CalcOptions : record
        {метод расчета типа "обычный", "25°С", или "peak-to-peak"
        копируется в момент расчета}
        CalcMethod : TCalcMethod;
        {Коэффициент отношения Номинал/ТЧХ при расчете оценки для метода "25°С"}
        {копируется в момент расчета}
        cm25Coeff : double;
        {веса регисров. копируется в момент расчета
        это доблы!!!!}
        RegistersWeighs : TRegistersDouble;
      end;
      constructor Create;
      destructor Destroy; override;
      procedure ReadFromCardOptions(const ASource : TmasProcessDBOptions);
      procedure Read(const ASource : TMemIniFile; const AFormatSettings: TFormatSettings);
      procedure Write(const ADest : TMemIniFile; const AFormatSettings: TFormatSettings);
      procedure Assign(const ASource : TIterOptions);
    end;

    TMeasErrorDescript = (medVvarCurr, medFreqCurr, medFreqCurve, medCharact, medVvarForce, medFreqQuarz, medFreqFinal);

    const
    MeasErrorDescriptSection : array[TMeasErrorDescript] of String =
                                              ('MeasErrorVvarCurr', 'MeasErrorFreqCurr',
                                              'MeasErrorFreqCurve','MeasErrorCharact',
                                              'MeasErrorVvarForce', 'MeasErrorFreqQuarz',
                                              'MeasErrorFreqFinal');
    type
    TMedItem = record
      Descript : TMeasErrorDescript;
      TotalCount,
      ErrCount : Word;
      function Percents : double;
    end;
    pMedItem = ^TMedItem;
    TErrorsObj = class(TList)
     private
      procedure Read(AIniFile : TMemIniFile; const ASecSuff : string = '');
      procedure Write(AIniFile : TMemIniFile; const ASecSuff : string = '');
     protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
      function Get(Index: Integer): pMedItem;
     public
      function NewItem(Descript: TMeasErrorDescript) : pMedItem;
      function First : pMedItem; reintroduce;
      function Last : pMedItem; reintroduce;
      procedure Clear; override;
      property Items[Index: Integer]: pMedItem read Get; default;
      function Find(ADescript : TMeasErrorDescript; out oItem : pMedItem):boolean;
      function TotalCount : word;
      function TotalErrorCount : word;
    end;
    TFieldOption = (foOptions, foRange, foMeas, foMeasCalc, foIterCalc, foResults, foNextOptions);
    TFieldOptions = set of TFieldOption;
    TIOArg = record
      Option : TFieldOption;
      SourceIni : TMemIniFile;
    end;
    TIOArgs = array of TIOArg;
    TMasIterationBase = class(TIteration)
     public
      class function IORead(const ADB : TEmbSQLDataBase; ANum : Word; const Args : TIOArgs):boolean;
      class function IOWrite(const ADB : TEmbSQLDataBase; ANum : Word; const Args : TIOArgs):boolean;
      class function CreateRecord(const ADB: TEmbSQLDataBase; out oNum: Word): Boolean;overload;  override;
      class function CreateTables(const ADB: TEmbSQLDataBase): boolean; override;
      class function Delete(const ADB: TEmbSQLDataBase; ANum: Word): boolean; override;
      class function ReadCount(const ADB: TEmbSQLDataBase): Word; override;
      class function ReadState(const ADB: TEmbSQLDataBase; ANum: Word; out oState : TIterStates): boolean; override;
      class function WriteState(const ADB: TEmbSQLDataBase; ANum: Word; AState: TIterStates): boolean; override;
    end;

    TMasIterationRegisters= class(TObject)
     private
      fRegisters : TMas6279D8Registers;
      fIteration : TMasIterationBase;
     protected
      procedure AfterCreate;virtual;
     public
      Results : record
       dF, dF25: double;
      end;
      constructor Create(const AIteration : TMasIterationBase); reintroduce; virtual;
      destructor Destroy;override;
      procedure Read(const ASource : TMemIniFile; const ASection : string; const AFormatSettings: TFormatSettings); virtual;
      procedure Write(const ADest : TMemIniFile; const ASection : string; const AFormatSettings: TFormatSettings); virtual;
      property Registers : TMas6279D8Registers read fRegisters;
      property Iteration : TMasIterationBase read fIteration;
    end;

    TMeasures<T : TMasIterationRegisters> = class(TObjectList<T>)
     private
      fIteration : TMasIterationBase;
      function GetItem(AIndex : Word):T;
     public
      constructor Create(const AIteration : TMasIterationBase); reintroduce; overload; virtual;
      constructor Create(const AIteration : TMasIterationBase; const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); reintroduce; overload; virtual;
      constructor Create(const AIteration : TMasIterationBase; const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); reintroduce; overload; virtual;
      function NewItem: T; overload;
      function NewItem(AIndex : Word) : T; overload;
      procedure Read(const ASource : TMemIniFile; const ASectionPreff : string; const AFormatSettings: TFormatSettings); virtual;
      procedure Write(const ADest : TMemIniFile; const ASectionPreff : string; const AFormatSettings: TFormatSettings); virtual;
      property Items[AIndex : Word]:T  read GetItem; default;
      property Iteration : TMasIterationBase read fIteration;
    end;

    TSolution = class(TMasIterationRegisters)
     private
      fRootIdx : Byte;
     public
      {RootIdx : byte;
      CalcResults : record
       dReg, Prior : double;
      end; }
      constructor Create(const AIteration : TMasIterationBase; const ARootIdx : Byte); reintroduce; virtual;
      {procedure Read(const ASource : TMemIniFile; const ASection : string; const AFormatSettings: TFormatSettings); override; }
      procedure Write(const ADest : TMemIniFile; const ASection : string; const AFormatSettings: TFormatSettings); override;
      property RootIdx : Byte read fRootIdx;
    end;


    TSolutions<T : TSolution> = class(TMeasures<T>)
    public
      function NewItem(const ARootIdx : Byte): T; overload;
      function NewItem(const ARootIdx : Byte; AIndex : Word) : T; overload;
      procedure Read(const ASource : TMemIniFile; const ASectionPreff : string; const AFormatSettings: TFormatSettings); override;
    end;

    TMeasure<TCalc : TSolution> = class(TMasIterationRegisters)
     strict private
      {список расчетных решений.
      заполняется и сохраняется в БД позиции процессом расчета.}
      fCalcSolutions : TSolutions<TCalc>;
      fErrors : TErrorsObj;
     public
      constructor Create(const AIteration : TMasIterationBase);override;
      destructor Destroy; override;
      property CalcSolutions : TSolutions<TCalc> read fCalcSolutions;
      function NewCalc : TCalc;
      property Errors : TErrorsObj read fErrors;
      procedure Read(const ASource : TMemIniFile; const ASection : string; const AFormatSettings: TFormatSettings); override;
      procedure Write(const ADest : TMemIniFile; const ASection : string; const AFormatSettings: TFormatSettings); override;
    end;



    TMasMeasPoint = class(TObject)
      PatternID : Integer;
      PointValue : Double;
      PatternExists,
      MeasInf,
      WorkPoint :Boolean;
      MeasSucc : TInRangeOptions;
      procedure MeasSuccInclude(AOption : TInRangeProcOption);
    end;

    TIterPointsList = class(TObjectList<TMasMeasPoint>)
     private
      fIteration : TMasIterationBase;
     public
      constructor Create(const AOwner: TMasIterationBase); reintroduce;
      function Find(Value, Precis : double; var Index : Word):boolean;
      function PointExists(Value, Precis : double):boolean;
      function WorksCount :Word;
      function SuccCount : Word;
      procedure CopyFrom(ASource : TList<TInPointOption>);
      procedure Read(const ASource : TMemIniFile; const AFormatSettings: TFormatSettings);
      procedure Write(const ADest : TMemIniFile; const AFormatSettings: TFormatSettings);
    end;

    TMasIteration<TCalc : TSolution;
                          TMeas : TMeasure<TCalc>>  = class(TMasIterationBase)
     private
      fMeasPoints : TIterPointsList;
      fOptions,
      fNextIterOptions : TIterOptions;
      fErrors : TErrorsObj;
      fMeasSolutions : TMeasures<TMeas>;
      fCalcResults : TSolutions<TCalc>;
     public
      constructor Create(const ADBExt : TPositionDBExtention);override;
      destructor Destroy; override;
      property MeasPoints : TIterPointsList read fMeasPoints;
      property NextIterOptions : TIterOptions read fNextIterOptions;
      property Options : TIterOptions read fOptions;
      property Errors : TErrorsObj read fErrors;
      property MeasSolutions : TMeasures<TMeas> read fMeasSolutions;
      property CalcResults : TSolutions<TCalc> read fCalcResults;
      function Read(const ADB: TEmbSQLDataBase; ANum : Word): boolean; override;
      function Read(const ADB: TEmbSQLDataBase; ANum : Word;
                    const AFormatSettings : TFormatSettings): boolean; override;
      function Write(const ADB: TEmbSQLDataBase; AIterState : Boolean; AOptions : TFieldOptions):boolean;
      procedure MoveCalcToMeas;
    end;

    TUpdatedData = (udStartOptions, udInitRegisters, udNominal, udStepsResult, udCompensationResult);
    TUpdatedFields = set of TUpdatedData;

    TmasDbExtention = class(TPositionDBExtention)
     private
      fStartOptions : TIterOptions;
      fInitRegisters : TMas6279D8Registers;
     protected
      fFatalError : boolean;
      procedure AfterCreate; override;
      procedure BeforeDestroy; override;
     public
      class function DefaultIterationClass : TIterationClass;override;
      function ReadPosData(const ASource : TMemIniFile; const AFormatSettings : TFormatSettings):Boolean; override;
      function UpdatePosData(const ADB : TEmbSQLDataBase; AFields : TUpdatedFields) : Boolean; overload;
      function UpdatePosData(AFields : TUpdatedFields) : Boolean; overload;
      property StartOptions : TIterOptions read fStartOptions;
      property InitRegisters : TMas6279D8Registers read fInitRegisters;
      property FatalError : boolean read fFatalError;
    end;

    {Vvar Curr}
    TVCError = (vmcProtect, vmcVoltOld, mvcVoltRange, mvcTempr, vmcVDD_VC);
    TVCErrors = set of TVCError;
    TVCErrorsAdapter = record
    case integer of
      0: (s: TVCErrors);
      1: (i: SmallInt);
    end;

    {Vvar Force/ Freq characterization}
    TVFError = (fceProtect, fceVoltOld, fceMeasError, fceDevMax, fceDispMax, fceTempr, fceVDD_VC, fceNominalOutVvar);
    TVFErrors = set of TVFError;
    TVFErrorsAdapter = record
    case integer of
      0: (s: TVFErrors);
      1: (i: SmallInt);
    end;

    {Vvar characterization}
    TVChError = (mveProtect, mveVoltOld, mveVoltRange, mveTempr, vmeVDD_VC);
    TVChErrors = set of TVChError;
    TVChErrorsAdapter = record
    case integer of
      0: (s: TVChErrors);
      1: (i: SmallInt);
    end;

    {TC OFF}
    TTOError = (toeProtect, toeVoltOld, toeMeasError, toeDevMax, toeDispMax, toeTempr, toeVDD_VC);
    TTOErrors = set of TTOError;
    TTOErrorsAdapter = record
    case integer of
      0: (s: TTOErrors);
      1: (i: SmallInt);
    end;

    {Freq Curr}
    TFMError = (fmeProtect, fmeVoltOld, fmeMeasError, fmeDevMax, fmeDispMax, fmeTempr, fmeVDD_VC);
    TFMErrors = set of TFMError;

    TFMErrorsAdapter = record
    case integer of
      0: (s: TFMErrors);
      1: (i: SmallInt);
    end;

    {Final Test}
    TFTError = (fteProtect, fteVoltOld, fteMeasError, fteDevMax, fteDispMax, fteTempr, fteVDD_VC);
    TFTErrors = set of TFTError;
    TFTErrorsAdapter = record
    case integer of
      0: (s: TFTErrors);
      1: (i: SmallInt);
    end;
implementation

uses

MAS6279D8.Consts,
System.Math,
Vodopad.CustomIniHelpers;



constructor TmasProcessDBOptions.Create;
begin
  inherited;
  ControlOptions.InPointOptions := TList<TInPointOption>.Create;
end;

destructor TmasProcessDBOptions.Destroy;
begin
  FreeAndNil(ControlOptions.InPointOptions);
  inherited;
end;

procedure TmasProcessDBOptions.Read(const ASource: TMemIniFile;
  const AFormatSettings: TFormatSettings);
var
vMm : TMeasureMode;
begin
  inherited;
  ControlOptions.InfPoint := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ControlOptions',
                            '', 25, AFormatSettings);
  ControlOptions.AllowChangeCDACNominal := ASource.ReadBool('Stp8ControlOptions','AllowChangeCDACNominal', false);
  ControlOptions.MeasureMode  := TMeasureMode(ASource.ReadInteger('Stp8ControlOptions','MeasureMode', 0));
  ControlOptions.StartTestOptions.i  := ASource.ReadInteger('Stp8ControlOptions','StartTestOptions', 0);
  ControlOptions.PreparePointOptions.i  := ASource.ReadInteger('Stp8ControlOptions','PreparePointOptions', 0);
  ControlOptions.PowerTurnOffOnInFinalTest := ASource.ReadBool('Stp8ControlOptions','PowerTurnOffOnInFinalTest', False);
  for vMm := Low(vMm) to High(vMm) do
    ControlOptions.InRangeOptions[vMm].i  := ASource.ReadInteger('Stp8ControlOptions',Format('InRangeOptions%d',[Ord(vMm)]),0);
  ControlOptions.AfterCompensOptions.i  := ASource.ReadInteger('Stp8ControlOptions','AfterCompensOptions',0);
  ErrorLimits.ppmMaxDeviation  := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'ppmMaxDeviation', 5, AFormatSettings);
  ErrorLimits.ppmMaxDispersion  := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'ppmMaxDispersion', 5, AFormatSettings);
  ErrorLimits.VDDTolerance  := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'VDD', 0.3, AFormatSettings);
  ErrorLimits.VCTolerance  := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'VC', 0.3, AFormatSettings);
  ErrorLimits.VvarMin  := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'VvarMin', 0.2, AFormatSettings);
  ErrorLimits.VvarMax   := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'VvarMax', 2.5, AFormatSettings);
  CalcOptions.CalcMethod := TCalcMethod(ASource.ReadInteger('Stp8CalcOptions','CalcMethod',0));
  CalcOptions.MaxMeasuresCalcCount := ASource.ReadInteger('Stp8CalcOptions','MaxMeasuresCalcCount',0);
  CalcOptions.MaxNextIterCalcCount  := ASource.ReadInteger('Stp8CalcOptions','MaxNextIterCalcCount',0);
  CalcOptions.ItersBeforeProgCount := ASource.ReadInteger('Stp8CalcOptions','ItersBeforeProgCount',0);
  CalcOptions.cm25Coeff  := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8CalcOptions',
                            'cm25Coeff', 0, AFormatSettings);
end;

procedure TmasProcessDBOptions.Assign(const ASource: TStp8ProcessDBOptions);
var
vSource : TmasProcessDBOptions;
vIdx : Byte;
vOp : TInPointOption;
vMm : TMeasureMode;
begin
  inherited;
  if not (ASource is TmasProcessDBOptions) then
    Exit;
  vSource := ASource as TmasProcessDBOptions;
  ControlOptions.InPointOptions.Clear;
  if (vSource.ControlOptions.InPointOptions.Count > 0) then
    for vIdx := 0 to vSource.ControlOptions.InPointOptions.Count - 1 do
    begin
      vOp.Assign(vSource.ControlOptions.InPointOptions[vIdx]);
      ControlOptions.InPointOptions.Add(vOp);
    end;
  ControlOptions.InfPoint := vSource.ControlOptions.InfPoint;
  ControlOptions.AllowChangeCDACNominal := vSource.ControlOptions.AllowChangeCDACNominal;
  ControlOptions.StartRegistersPatternID := vSource.ControlOptions.StartRegistersPatternID;
  ControlOptions.AdditionalMeasurePatternID := vSource.ControlOptions.AdditionalMeasurePatternID;
  ControlOptions.MeasureMode := vSource.ControlOptions.MeasureMode;
  ControlOptions.StartTestOptions.i := vSource.ControlOptions.StartTestOptions.i;
  ControlOptions.PreparePointOptions.i := vSource.ControlOptions.PreparePointOptions.i;
  for vMM := Low(vMm) to High(vMm) do
    ControlOptions.InRangeOptions[vMm].i := vSource.ControlOptions.InRangeOptions[vMm].i;
  ControlOptions.AfterCompensOptions.i := vSource.ControlOptions.AfterCompensOptions.i;
  ControlOptions.PowerTurnOffOnInFinalTest := vSource.ControlOptions.PowerTurnOffOnInFinalTest;
  ErrorLimits.ppmMaxDeviation := vSource.ErrorLimits.ppmMaxDeviation;
  ErrorLimits.ppmMaxDispersion := vSource.ErrorLimits.ppmMaxDispersion;
  ErrorLimits.VvarMin := vSource.ErrorLimits.VvarMin;
  ErrorLimits.VvarMax := vSource.ErrorLimits.VvarMax;
  CalcOptions.RegWeighPatternID := vSource.CalcOptions.RegWeighPatternID;
  CalcOptions.CalcMethod := vSource.CalcOptions.CalcMethod;
  CalcOptions.MaxMeasuresCalcCount := vSource.CalcOptions.MaxMeasuresCalcCount;
  CalcOptions.MaxNextIterCalcCount := vSource.CalcOptions.MaxNextIterCalcCount;
  CalcOptions.ItersBeforeProgCount := vSource.CalcOptions.ItersBeforeProgCount;
  CalcOptions.cm25Coeff := vSource.CalcOptions.cm25Coeff;
end;

function TmasProcessDBOptions.Equals(const ASource: TStp8ProcessDBOptions): Boolean;
var
vSource : TmasProcessDBOptions;
i : Byte;
vA, vB : TInPointOption;
vMm : TMeasureMode;
begin
  Result := inherited Equals(ASource);
  if Result then
    Result := (ASource is TmasProcessDBOptions);
  if not Result then
    Exit;
  vSource := ASource as TmasProcessDBOptions;
  Result := SameValue(ControlOptions.InfPoint, vSource.ControlOptions.InfPoint, 0.001)
    and (ControlOptions.AllowChangeCDACNominal = vSource.ControlOptions.AllowChangeCDACNominal)
    and (ControlOptions.StartRegistersPatternID = vSource.ControlOptions.StartRegistersPatternID)
    and (ControlOptions.AdditionalMeasurePatternID = vSource.ControlOptions.AdditionalMeasurePatternID)
    and (ControlOptions.MeasureMode = vSource.ControlOptions.MeasureMode)
    and (ControlOptions.StartTestOptions.i = vSource.ControlOptions.StartTestOptions.i)
    and (ControlOptions.PreparePointOptions.i = vSource.ControlOptions.PreparePointOptions.i)
    and (ControlOptions.AfterCompensOptions.i = vSource.ControlOptions.AfterCompensOptions.i)
    and (ControlOptions.PowerTurnOffOnInFinalTest = vSource.ControlOptions.PowerTurnOffOnInFinalTest)
    and SameValue(ErrorLimits.ppmMaxDeviation, vSource.ErrorLimits.ppmMaxDeviation, 0.001)
    and SameValue(ErrorLimits.ppmMaxDispersion, vSource.ErrorLimits.ppmMaxDispersion, 0.001)
    and SameValue(ErrorLimits.VvarMin, vSource.ErrorLimits.VvarMin, 0.001)
    and SameValue(ErrorLimits.VvarMax, vSource.ErrorLimits.VvarMax, 0.001)
    and (CalcOptions.RegWeighPatternID = vSource.CalcOptions.RegWeighPatternID)
    and (CalcOptions.CalcMethod = vSource.CalcOptions.CalcMethod)
    and (CalcOptions.MaxMeasuresCalcCount = vSource.CalcOptions.MaxMeasuresCalcCount)
    and (CalcOptions.MaxNextIterCalcCount = vSource.CalcOptions.MaxNextIterCalcCount)
    and (CalcOptions.ItersBeforeProgCount = vSource.CalcOptions.ItersBeforeProgCount)
    and SameValue(CalcOptions.cm25Coeff, vSource.CalcOptions.cm25Coeff, 0.001);
  if result then
  begin
    for vMm := Low(vMm) to High(vMm) do
    begin
      result := (ControlOptions.InRangeOptions[vMM].i = vSource.ControlOptions.InRangeOptions[vMm].i);
      if not Result then
        Break;
    end;
  end;
  if Result then
  begin
    result := ControlOptions.InPointOptions.Count = vSource.ControlOptions.InPointOptions.Count;
    if not Result then
      Exit;
    Result := False;
    i := 0;
    while (i < ControlOptions.InPointOptions.Count) do
    begin
      vA := vSource.ControlOptions.InPointOptions[i];
      vB := ControlOptions.InPointOptions[i];
      result := (vA = vB);
      if not Result then
        break;
      inc(i);
    end;
  end;
end;

procedure TmasProcessDBOptions.Write(const ADest: TMemIniFile;
  const AFormatSettings: TFormatSettings);
var
vMm : TMeasureMode;
begin
  inherited;
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ControlOptions',
                            'InfPoint', ControlOptions.InfPoint, AFormatSettings);
  ADest.WriteBool('Stp8ControlOptions','AllowChangeCDACNominal',ControlOptions.AllowChangeCDACNominal);
  ADest.WriteInteger('Stp8ControlOptions','MeasureMode',Ord(ControlOptions.MeasureMode));
  ADest.WriteInteger('Stp8ControlOptions','StartTestOptions',ControlOptions.StartTestOptions.i);
  ADest.WriteInteger('Stp8ControlOptions','PreparePointOptions',ControlOptions.PreparePointOptions.i);
  for vMm := Low(vMm) to High(vMm) do
    ADest.WriteInteger('Stp8ControlOptions', Format('InRangeOptions%d',[Ord(vMm)]),ControlOptions.InRangeOptions[vMm].i);
  ADest.WriteInteger('Stp8ControlOptions','AfterCompensOptions',ControlOptions.AfterCompensOptions.i);
  ADest.WriteBool('Stp8ControlOptions','PowerTurnOffOnInFinalTest', ControlOptions.PowerTurnOffOnInFinalTest);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'ppmMaxDeviation', ErrorLimits.ppmMaxDeviation, AFormatSettings);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'ppmMaxDispersion', ErrorLimits.ppmMaxDispersion, AFormatSettings);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'VDD', ErrorLimits.VDDTolerance, AFormatSettings);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'VC', ErrorLimits.VCTolerance, AFormatSettings);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'VvarMin', ErrorLimits.VvarMin, AFormatSettings);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'VvarMax', ErrorLimits.VvarMax, AFormatSettings);
  ADest.WriteInteger('Stp8CalcOptions','CalcMethod',Ord(CalcOptions.CalcMethod));
  ADest.WriteInteger('Stp8CalcOptions','MaxMeasuresCalcCount',CalcOptions.MaxMeasuresCalcCount);
  ADest.WriteInteger('Stp8CalcOptions','MaxNextIterCalcCount',CalcOptions.MaxNextIterCalcCount);
  ADest.WriteInteger('Stp8CalcOptions','ItersBeforeProgCount',CalcOptions.ItersBeforeProgCount);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8CalcOptions',
                            'cm25Coeff',
                            CalcOptions.cm25Coeff, AFormatSettings);
end;


{ TInPointOption }

procedure TInPointOption.Assign(ASource: TInPointOption);
begin
  PointID := ASource.PointID;
  PatternID := ASource.PatternID;
  PointValue := ASource.PointValue;
  PatternExists := ASource.PatternExists;
  MeasInf := ASource.MeasInf;
  WorkPoint := ASource.WorkPoint;
end;

class operator TInPointOption.Equal(const A, B : TInPointOption) : Boolean;
begin
  result := (A.PointID = B.PointID)
  and (A.PatternID = B.PatternID)
  and (SameValue(A.PointValue, B.PointValue, 0.001))
  and (A.PatternExists = B.PatternExists)
  and (A.MeasInf = B.MeasInf)
  and (A.WorkPoint = B.WorkPoint);
end;

{ TmasDbExtention }

procedure TmasDbExtention.AfterCreate;
begin
  inherited;
  fStartOptions := TIterOptions.Create;
  fInitRegisters := TMas6279D8Registers.Create(nil);
  fFatalError := false;
end;

procedure TmasDbExtention.BeforeDestroy;
begin
  FreeAndNil(fInitRegisters);
  FreeAndNil(fStartOptions);
  inherited;
end;

class function TmasDbExtention.DefaultIterationClass: TIterationClass;
begin
  Result := TMasIteration<TSolution, TMeasure<TSolution>>;
end;

function TmasDbExtention.ReadPosData(const ASource: TMemIniFile;
  const AFormatSettings: TFormatSettings): Boolean;
begin
  result := inherited ReadPosData(ASource, AFormatSettings);
  if result then
  begin
    fFatalError := ASource.ReadBool('FatalError','State', false);
    StartOptions.Read(ASource, AFormatSettings);
    InitRegisters.LoadFromIni(ASource, 'stp8InitRegisters');
  end;
end;

function TmasDbExtention.UpdatePosData(const ADB: TEmbSQLDataBase;
  AFields: TUpdatedFields): Boolean;
var
vIniFile : TMemIniFile;
vFormatSettings: TFormatSettings;
begin
  vFormatSettings := TFormatSettings.Create(1033);
  vIniFile := TMemIniFile.Create('');
  try
    Result := ReadPosData(ADB, vIniFile);
    if not Result then
      Exit;
    vIniFile.WriteBool('FatalError','State', fFatalError);
    if (udStartOptions in AFields) then
      fStartOptions.Write(vIniFile, vFormatSettings);
    if (udInitRegisters in AFields) then
      fInitRegisters.SaveToIni(vIniFile, 'stp8InitRegisters');
    if (udNominal in AFields) then
      UpdateNominal(vIniFile, Nominal, vFormatSettings);
    if (udStepsResult in AFields) then
      UpdateStepsResults(vIniFile, fStepsResults);
    if (udCompensationResult in AFields) then
      WriteCompensationResult(vIniFile, CompensationResult);
    Result := WritePosData(ADB, vIniFile);
  finally
    FreeAndNil(vIniFile);
  end;
end;

function TmasDbExtention.UpdatePosData(AFields : TUpdatedFields): Boolean;
var
vDB : TEmbSQLDataBase;
begin
  Result := False;
  if AFields = [] then
    Exit;
  vDB := CreateConnection;
  try
    Result := UpdatePosData(vDB, AFields);
  finally
    if vDB.TransactionActive then
      vDB.RollBack;
    vDB.DBClose;
    FreeAndNil(vDB);
  end;
end;

{ TIterOptions }

procedure TIterOptions.Assign(const ASource: TIterOptions);
var
vMm : TMeasureMode;
begin
  ControlOptions.MeasureMode  := ASource.ControlOptions.MeasureMode;
  ControlOptions.StartTestOptions.i  := ASource.ControlOptions.StartTestOptions.i;
  ControlOptions.PreparePointOptions.i  := ASource.ControlOptions.PreparePointOptions.i;
  for vMm := Low(vMm) to High(vMm) do
    ControlOptions.InRangeOptions[vMm].i  := ASource.ControlOptions.InRangeOptions[vMm].i;
  ControlOptions.AfterCompensOptions.i  := ASource.ControlOptions.AfterCompensOptions.i;

  ErrorLimits.VvarMin  := ASource.ErrorLimits.VvarMin;
  ErrorLimits.VvarMax   := ASource.ErrorLimits.VvarMax;
end;

constructor TIterOptions.Create;
begin
  inherited;
  CalcOptions.RegistersWeighs := TRegistersDouble.Create;
end;

destructor TIterOptions.Destroy;
begin
  FreeAndNil(CalcOptions.RegistersWeighs);
  inherited Destroy;
end;

procedure TIterOptions.Read(const ASource: TMemIniFile;
  const AFormatSettings: TFormatSettings);
var
vMm : TMeasureMode;
begin
  ControlOptions.MeasureMode  := TMeasureMode(ASource.ReadInteger('Stp8ControlOptions','MeasureMode',0));
  ControlOptions.StartTestOptions.i  := ASource.ReadInteger('Stp8ControlOptions','StartTestOptions',0);
  ControlOptions.PreparePointOptions.i  := ASource.ReadInteger('Stp8ControlOptions','PreparePointOptions',0);
  for vMm := Low(vMm) to High(vMm) do
    ControlOptions.InRangeOptions[vMm].i  := ASource.ReadInteger('Stp8ControlOptions',Format('InRangeOptions%d',[Ord(vMm)]),0);
  ControlOptions.AfterCompensOptions.i  := ASource.ReadInteger('Stp8ControlOptions','AfterCompensOptions',0);

  ErrorLimits.VvarMin  := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'VvarMin', 0.2, AFormatSettings);
  ErrorLimits.VvarMax   := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ErrorLimitsOptions',
                            'VvarMax', 2.5, AFormatSettings);
  CalcOptions.CalcMethod := TCalcMethod(ASource.ReadInteger('Stp8ControlOptions','CalcMethod',0));
  CalcOptions.cm25Coeff := Vodopad.CustomIniHelpers.ReadFloat(
                            ASource, 'Stp8ControlOptions',
                            'cm25Coeff', 1, AFormatSettings);
  CalcOptions.RegistersWeighs.Read(ASource, AFormatSettings);
end;

procedure TIterOptions.ReadFromCardOptions(const ASource : TmasProcessDBOptions);
var
vMm : TMeasureMode;
begin
  ControlOptions.MeasureMode  := ASource.ControlOptions.MeasureMode;
  ControlOptions.StartTestOptions.i  := ASource.ControlOptions.StartTestOptions.i;
  ControlOptions.PreparePointOptions.i  := ASource.ControlOptions.PreparePointOptions.i;
  for vMm := Low(vMm) to High(vMm) do
    ControlOptions.InRangeOptions[vMm].i  := ASource.ControlOptions.InRangeOptions[vMm].i;
  ControlOptions.AfterCompensOptions.i  := ASource.ControlOptions.AfterCompensOptions.i;

  ErrorLimits.VvarMin  := ASource.ErrorLimits.VvarMin;
  ErrorLimits.VvarMax   := ASource.ErrorLimits.VvarMax;
  CalcOptions.CalcMethod := ASource.CalcOptions.CalcMethod;
  CalcOptions.cm25Coeff := ASource.CalcOptions.cm25Coeff;
end;

procedure TIterOptions.Write(const ADest: TMemIniFile;
  const AFormatSettings: TFormatSettings);
var
vMm : TMeasureMode;
begin
  ADest.WriteInteger('Stp8ControlOptions','MeasureMode',Ord(ControlOptions.MeasureMode));
  ADest.WriteInteger('Stp8ControlOptions','StartTestOptions',ControlOptions.StartTestOptions.i);
  ADest.WriteInteger('Stp8ControlOptions','PreparePointOptions',ControlOptions.PreparePointOptions.i);
  for vMm := Low(vMm) to High(vMm) do
    ADest.WriteInteger('Stp8ControlOptions', Format('InRangeOptions%d',[Ord(vMm)]),ControlOptions.InRangeOptions[vMm].i);
  ADest.WriteInteger('Stp8ControlOptions','AfterCompensOptions',ControlOptions.AfterCompensOptions.i);

  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'VvarMin', ErrorLimits.VvarMin, AFormatSettings);
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ErrorLimitsOptions',
                            'VvarMax', ErrorLimits.VvarMax, AFormatSettings);
  ADest.WriteInteger('Stp8ControlOptions','CalcMethod', Ord(CalcOptions.CalcMethod));
  Vodopad.CustomIniHelpers.WriteFloat(
                            ADest, 'Stp8ControlOptions',
                            'cm25Coeff', CalcOptions.cm25Coeff, AFormatSettings);
  CalcOptions.RegistersWeighs.Write(ADest, AFormatSettings);
end;

{ TMasIteration }

{$REGION ' Tables '}
procedure FreqCurrCreateTable(const ADB: TEmbSQLDataBase);
begin
  ADB.Execute('CREATE TABLE IF NOT EXISTS FREQ_CURR (  '+
  '  ITER_NUM   INTEGER DEFAULT ( 0 )' +
  ', SOL_NUM    INTEGER DEFAULT ( 0 )' +
  ', IZM_NUM    INTEGER DEFAULT ( 0 ) ' +
  ', POINTIDX   INTEGER DEFAULT ( 0 ) ' +
  ', TEMPR      DOUBLE  DEFAULT ( 0 ) ' +
  ', VDD        DOUBLE  DEFAULT ( 0 ) ' +
  ', VC         DOUBLE  DEFAULT ( 0 ) ' +
  ', REGMEM     BLOB '+
  ', FREQ       DOUBLE  DEFAULT ( 0 ) ' +
  ', FREQDISP   DOUBLE  DEFAULT ( 0 ) ' +
  ', ERR_CODE   INTEGER DEFAULT ( -1 ) ' +
  ' ); ');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_SOL_NUM_ASC ON FREQ_CURR (SOL_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_SOL_NUM_DSC ON FREQ_CURR (SOL_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_IZM_NUM_ASC ON FREQ_CURR (IZM_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_IZM_NUM_DSC ON FREQ_CURR (IZM_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_ITER_NUM_ASC ON FREQ_CURR (ITER_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_ITER_NUM_DSC ON FREQ_CURR (ITER_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_SOL_NUM_ASC ON FREQ_CURR (SOL_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_CURR_SOL_NUM_DSC ON FREQ_CURR (SOL_NUM COLLATE BINARY DESC);');
end;

procedure VvarCurrCreateTable(const ADB: TEmbSQLDataBase);
begin
  ADB.Execute('CREATE TABLE IF NOT EXISTS VVAR_CURR (  '+
  '  ITER_NUM   INTEGER DEFAULT ( 0 )' +
  ', SOL_NUM    INTEGER DEFAULT ( 0 )' +
  ', IZM_NUM    INTEGER DEFAULT ( 0 ) ' +
  ', POINTIDX   INTEGER DEFAULT ( 0 ) ' +
  ', TEMPR      DOUBLE  DEFAULT ( 0 ) ' +
  ', VDD        DOUBLE  DEFAULT ( 0 ) ' +
  ', VC         DOUBLE  DEFAULT ( 0 ) ' +
  ', REGMEM     BLOB '+
  ', VOLTAGE    DOUBLE  DEFAULT ( 0 ) ' +
  ', ERR_CODE   INTEGER DEFAULT ( -1 ) ' +
  ' ); ');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_SOL_NUM_ASC ON VVAR_CURR (SOL_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_SOL_NUM_DSC ON VVAR_CURR (SOL_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_IZM_NUM_ASC ON VVAR_CURR (IZM_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_IZM_NUM_DSC ON VVAR_CURR (IZM_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_ITER_NUM_ASC ON VVAR_CURR (ITER_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_ITER_NUM_DSC ON VVAR_CURR (ITER_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_SOL_NUM_ASC ON VVAR_CURR (SOL_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CURR_SOL_NUM_DSC ON VVAR_CURR (SOL_NUM COLLATE BINARY DESC);');
end;

procedure FreqTC_OFF_CreateTable(const ADB: TEmbSQLDataBase);
begin
  ADB.Execute('CREATE TABLE IF NOT EXISTS FREQ_TCOFF (  '+
  '  ITER_NUM   INTEGER DEFAULT ( 0 )' +
  ', IZM_NUM    INTEGER DEFAULT ( 0 ) ' +
  ', POINTIDX   INTEGER DEFAULT ( 0 ) ' +
  ', TEMPR      DOUBLE  DEFAULT ( 0 ) ' +
  ', VDD        DOUBLE  DEFAULT ( 0 ) ' +
  ', VC         DOUBLE  DEFAULT ( 0 ) ' +
  ', REGMEM     BLOB '+
  ', FREQ       DOUBLE  DEFAULT ( 0 ) ' +
  ', FREQDISP   DOUBLE  DEFAULT ( 0 ) ' +
  ', ERR_CODE   INTEGER DEFAULT ( -1 ) ' +
  ' ); ');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_TCOFF_IZM_NUM_ASC ON FREQ_TCOFF (IZM_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_TCOFF_IZM_NUM_DSC ON FREQ_TCOFF (IZM_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_TCOFF_ITER_NUM_ASC ON FREQ_TCOFF (ITER_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_TCOFF_ITER_NUM_DSC ON FREQ_TCOFF (ITER_NUM COLLATE BINARY DESC);');
end;

procedure VvarCharCreateTable(const ADB: TEmbSQLDataBase);
begin
  ADB.Execute('CREATE TABLE IF NOT EXISTS VVAR_CHAR (  '+
  '  ITER_NUM   INTEGER DEFAULT ( 0 )' +
  ', SOL_NUM    INTEGER DEFAULT ( 0 )' +
  ', IZM_NUM    INTEGER DEFAULT ( 0 ) ' +
  ', POINTIDX   INTEGER DEFAULT ( 0 ) ' +
  ', TEMPR      DOUBLE  DEFAULT ( 0 ) ' +
  ', VDD        DOUBLE  DEFAULT ( 0 ) ' +
  ', VC         DOUBLE  DEFAULT ( 0 ) ' +
  ', METHOD_TAG INTEGER DEFAULT ( 0 ) ' +
  ', METHOD_VAR INTEGER DEFAULT ( 0 ) ' +
  ', REGMEM     BLOB '+
  ', VOLTAGE    DOUBLE  DEFAULT ( 0 ) ' +
  ', ERR_CODE   INTEGER DEFAULT ( -1 ) ' +
  ' ); ');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CHAR_SOL_NUM_ASC ON VVAR_CHAR (SOL_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CHAR_SOL_NUM_DSC ON VVAR_CHAR (SOL_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CHAR_IZM_NUM_ASC ON VVAR_CHAR (IZM_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CHAR_IZM_NUM_DSC ON VVAR_CHAR (IZM_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CHAR_ITER_NUM_ASC ON VVAR_CHAR (ITER_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_CHAR_ITER_NUM_DSC ON VVAR_CHAR (ITER_NUM COLLATE BINARY DESC);');
end;

procedure CreateFCTable(const ADB: TEmbSQLDataBase);
begin
  ADB.Execute('CREATE TABLE IF NOT EXISTS VVAR_FC (  '+
  '  ITER_NUM   INTEGER DEFAULT ( 0 )' +
  ', IZM_NUM    INTEGER DEFAULT ( 0 ) ' +
  ', POINTIDX   INTEGER DEFAULT ( 0 ) ' +
  ', TEMPR      DOUBLE  DEFAULT ( 0 ) ' +
  ', VDD        DOUBLE  DEFAULT ( 0 ) ' +
  ', VC         DOUBLE  DEFAULT ( 0 ) ' +
  ', VOLTAGE    DOUBLE  DEFAULT ( 0 ) ' +
  ', FREQ       DOUBLE  DEFAULT ( 0 ) ' +
  ', FREQDISP   DOUBLE  DEFAULT ( 0 ) ' +
  ', ERR_CODE   INTEGER DEFAULT ( -1 ) ' +
  ' ); ');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_FC_IZM_NUM_ASC ON VVAR_FC (IZM_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_FC_IZM_NUM_DSC ON VVAR_FC (IZM_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_FC_ITER_NUM_ASC ON VVAR_FC (ITER_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS VVAR_FC_ITER_NUM_DSC ON VVAR_FC (ITER_NUM COLLATE BINARY DESC);');
end;

procedure FreqFinalCreateTable(const ADB: TEmbSQLDataBase);
begin
  ADB.Execute('CREATE TABLE IF NOT EXISTS FREQ_FINAL (  '+
  '  ITER_NUM   INTEGER DEFAULT ( 0 )' +
  ', IZM_NUM    INTEGER DEFAULT ( 0 ) ' +
  ', POINTIDX   INTEGER DEFAULT ( 0 ) ' +
  ', TEMPR      DOUBLE  DEFAULT ( 0 ) ' +
  ', VDD        DOUBLE  DEFAULT ( 0 ) ' +
  ', VC         DOUBLE  DEFAULT ( 0 ) ' +
  ', FREQ       DOUBLE  DEFAULT ( 0 ) ' +
  ', FREQDISP   DOUBLE  DEFAULT ( 0 ) ' +
  ', ERR_CODE   INTEGER DEFAULT ( -1 ) ' +
  ' ); ');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_FINAL_IZM_NUM_ASC ON FREQ_FINAL (IZM_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_FINAL_IZM_NUM_DSC ON FREQ_FINAL (IZM_NUM COLLATE BINARY DESC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_FINAL_ITER_NUM_ASC ON FREQ_FINAL (ITER_NUM COLLATE BINARY ASC);');
  ADB.Execute(
  'CREATE INDEX IF NOT EXISTS FREQ_FINAL_ITER_NUM_DSC ON FREQ_FINAL (ITER_NUM COLLATE BINARY DESC);');
end;

procedure FreqCurrDeleteIter(const ADB: TEmbSQLDataBase; AIter: Word);
const vSqlText =
      'DELETE FROM FREQ_CURR WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
begin
  ADB.TransactionBegin();
  vQR.Prepare(ADB.DB, vSqlText);
  vQR.Bind(1, AIter);
  vQR.Execute;
  ADB.Commit;
end;

procedure VvarCurrDeleteIter(const ADB: TEmbSQLDataBase; AIter: Word);
const vSqlText =
      'DELETE FROM VVAR_CURR WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
begin
  ADB.TransactionBegin();
  vQR.Prepare(ADB.DB, vSqlText);
  vQR.Bind(1, AIter);
  vQR.Execute;
  ADB.Commit;
end;

procedure VvarCharDeleteIter(const ADB: TEmbSQLDataBase; AIter: Word);
const vSqlText =
      'DELETE FROM VVAR_CHAR WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
begin
  ADB.TransactionBegin();
  vQR.Prepare(ADB.DB, vSqlText);
  vQR.Bind(1, AIter);
  vQR.Execute;
  ADB.Commit;
end;

procedure FCMeasDeleteIter(const ADB: TEmbSQLDataBase; AIter: Word);
const vSqlText =
      'DELETE FROM VVAR_FC WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
begin
  ADB.TransactionBegin();
  vQR.Prepare(ADB.DB, vSqlText);
  vQR.Bind(1, AIter);
  vQR.Execute;
  ADB.Commit;
end;

procedure FreqTC_OFF_DeleteIter(const ADB: TEmbSQLDataBase; AIter: Word);
const vSqlText =
      'DELETE FROM FREQ_TCOFF WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
begin
  ADB.TransactionBegin();
  vQR.Prepare(ADB.DB, vSqlText);
  vQR.Bind(1, AIter);
  vQR.Execute;
  ADB.Commit;
end;

procedure FreqFinalDeleteIter(const ADB: TEmbSQLDataBase; AIter: Word);
const vSqlText =
      'DELETE FROM FREQ_FINAL WHERE ITER_NUM = :ITER_NUM';
var
vQR : TEmbSQLRequest;
begin
  ADB.TransactionBegin();
  vQR.Prepare(ADB.DB, vSqlText);
  vQR.Bind(1, AIter);
  vQR.Execute;
  ADB.Commit;
end;
{$ENDREGION}

{TMasIterationCustom}

class function TMasIterationBase.CreateRecord(const ADB: TEmbSQLDataBase; out oNum : Word): Boolean;
const
c_SelMaxNumSqlText = 'SELECT MAX(NUM) FROM ITERATIONS';
c_InsertSqlText = 'INSERT INTO ITERATIONS (NUM) VALUES (:NUM);';
var
vQR : TEmbSQLRequest;
begin
  oNum := 1;
  try
    ADB.TransactionBegin();
    vQR.Prepare(ADB.DB, c_SelMaxNumSqlText);
    if (vQR.PrepareNext = SQLEMB_DONE)
      and (vQR.Step = SQLEMB_ROW) then
      oNum := vQR.FieldInt(0)+1;
    vQR.Close;
    vQR.Prepare(ADB.DB, c_InsertSqlText);
    vQR.Bind(1, oNum);
    vQR.Execute;
    ADB.Commit;
    result := true;
    vQR.Close;
  except
    result := false;
    if ADB.TransactionActive then
      ADB.Rollback;
  end;
end;

class function TMasIterationBase.CreateTables(const ADB: TEmbSQLDataBase): boolean;
begin
  try
    ADB.TransactionBegin;
    ADB.Execute('CREATE TABLE IF NOT EXISTS ITERATIONS (  '+
    '  NUM        INTEGER NOT NULL  DEFAULT ( 0 ) ' +
    ', STATE      INTEGER NOT NULL  DEFAULT ( 0 ) ' +
    ', OPTIONS         BLOB '+
    ', RANGEDATA       BLOB '+
    ', MEASDATA        BLOB '+
    ', MEASCALCDATA    BLOB '+
    ', ITERCALCDATA    BLOB '+
    ', RESULTS         BLOB '+
    ', NEXTOPTIONS     BLOB '+
    ' ); ');
    ADB.Execute(
    'CREATE INDEX IF NOT EXISTS ITERATIONS_NUM_ASC ON ITERATIONS (NUM COLLATE BINARY ASC);');
    ADB.Execute(
    'CREATE INDEX IF NOT EXISTS ITERATIONS_NUM_DESC ON ITERATIONS (NUM COLLATE BINARY DESC);');
    FreqCurrCreateTable(ADB);
    VvarCurrCreateTable(ADB);
    VvarCharCreateTable(ADB);
    CreateFCTable(ADB);
    FreqTC_OFF_CreateTable(ADB);
    FreqFinalCreateTable(ADB);
    ADB.Commit;
    result := true;
  except
    result := false;
    if ADB.TransactionActive then
      ADB.Rollback;
  end;
end;

class function TMasIterationBase.Delete(const ADB: TEmbSQLDataBase; ANum: Word): boolean;
const
c_SqlText = 'DELETE FROM ITERATIONS WHERE NUM = :NUM';
var
vQR : TEmbSQLRequest;
begin
  try
    FreqCurrDeleteIter(ADB, ANum);
    VvarCurrDeleteIter(ADB, ANum);
    VvarCharDeleteIter(ADB, ANum);
    FCMeasDeleteIter(ADB, ANum);
    FreqTC_OFF_DeleteIter(ADB, ANum);
    FreqFinalDeleteIter(ADB, ANum);
    ADB.TransactionBegin();
    vQR.Prepare(ADB.DB, c_SqlText);
    vQR.Bind(1, ANum);
    vQR.Execute;
    ADB.Commit;
    ADB.ExecuteNoException('VACUUM');
    result := true;
  except
    result := false;
    if ADB.TransactionActive then
      ADB.Rollback;
  end;
end;

class function TMasIterationBase.ReadCount(const ADB: TEmbSQLDataBase): Word;
const
c_SqlText = 'SELECT COUNT(*) FROM ITERATIONS';
var
vQR : TEmbSQLRequest;
begin
  Result := 0;
  try
    vQR.Prepare(ADB.DB, c_SqlText);
    if (vQR.PrepareNext = SQLEMB_DONE)
      and (vQR.Step = SQLEMB_ROW) then
      Result := vQR.FieldInt(0);
    vQR.Close;
  except
    Result := 0;
  end;
end;

class function TMasIterationBase.ReadState(const ADB: TEmbSQLDataBase; ANum : Word; out oState : TIterStates): boolean;
const
c_SqlText = 'SELECT STATE FROM ITERATIONS WHERE NUM = :NUM';
var
vQR : TEmbSQLRequest;
vAd : TIterStatesAdapter;
begin
  result := false;
  oState := [];
  try
    vQR.Prepare(ADB.DB, c_SqlText);
    vQR.Bind(1, ANum);
    if (vQR.PrepareNext = SQLEMB_DONE)
      and (vQR.Step = SQLEMB_ROW) then
    begin
      vAd.b := vQR.FieldInt(0);
      oState := vAd.s;
      Result := True;
    end;
    vQR.Close;
  except
    oState := [];
    result := false;
  end;
end;

class function TMasIterationBase.WriteState(const ADB: TEmbSQLDataBase; ANum: Word;
  AState: TIterStates): boolean;
const
c_SqlText = 'UPDATE ITERATIONS SET STATE = :STATE WHERE NUM = :NUM';
var
vQR : TEmbSQLRequest;
vAd : TIterStatesAdapter;
begin
  Result := false;
  vAd.s := AState;
  try
    ADB.TransactionBegin;
    vQR.Prepare(ADB.DB, c_SqlText);
    vQR.Bind(1, vAd.b);
    vQR.Bind(2, ANum);
    vQR.Execute;
    ADB.Commit;
    Result := true;
  except
    Result := false;
    vQR.Close;
    if ADB.TransactionActive then
      ADB.Rollback;
  end;
end;

class function TMasIterationBase.IORead(const ADB : TEmbSQLDataBase; ANum : Word; const Args : TIOArgs): boolean;
const
C_SelOptions = 'SELECT OPTIONS FROM ITERATIONS WHERE NUM = :NUM;';
C_SelRange = 'SELECT RANGEDATA FROM ITERATIONS WHERE NUM = :NUM;';
C_SelMeas = 'SELECT MEASDATA FROM ITERATIONS WHERE NUM = :NUM;';
C_SelMeasCalc = 'SELECT MEASCALCDATA FROM ITERATIONS WHERE NUM = :NUM;';
C_SelIterCalc = 'SELECT ITERCALCDATA FROM ITERATIONS WHERE NUM = :NUM;';
C_SelResults = 'SELECT RESULTS FROM ITERATIONS WHERE NUM = :NUM;';
C_SelNextOptions = 'SELECT NEXTOPTIONS FROM ITERATIONS WHERE NUM = :NUM;';
var
vQR : TEmbSQLRequest;
vArgsIdx : Byte;
vMs : TMemoryStream;
vStr : TStringList;
begin
  Result := false;
  if(Length(Args) < 1) then
    Exit;
  vMs := TMemoryStream.Create;
  vStr := TStringList.Create;
  try
    try
      for vArgsIdx := Low(Args) to High(Args) do
      begin
        vStr.Clear;
        vMs.Clear;
        vMs.Position := 0;
        case Args[vArgsIdx].Option of
          foOptions: vQR.Prepare(ADB.DB, C_SelOptions);
          foRange: vQR.Prepare(ADB.DB, C_SelRange);
          foMeas:  vQR.Prepare(ADB.DB, C_SelMeas);
          foMeasCalc: vQR.Prepare(ADB.DB, C_SelMeasCalc);
          foIterCalc: vQR.Prepare(ADB.DB, C_SelIterCalc);
          foResults: vQR.Prepare(ADB.DB, C_SelResults);
          foNextOptions: vQR.Prepare(ADB.DB, C_SelNextOptions);
        end;
        vQR.Bind(1, ANum);
        if (vQR.PrepareNext = SQLEMB_DONE)
        and (vQR.Step = SQLEMB_ROW) then
        begin
          vQR.SaveBlobFieldToStream(0, vMs);
          vMs.Position := 0;
          vStr.Clear;
          vStr.LoadFromStream(vMs);
          Args[vArgsIdx].SourceIni.SetStrings(vStr);
        end;
        vQR.Close;
      end;
      Result := True;
    except
      Result := false;
    end;
  finally
    FreeAndNil(vMs);
    FreeAndNil(vStr);
  end;
end;

class function TMasIterationBase.IOWrite(const ADB : TEmbSQLDataBase; ANum : Word; const Args : TIOArgs): boolean;
const
c_SqlText = 'SELECT STATE FROM ITERATIONS WHERE NUM = :NUM;';
C_UpdateOptions = 'UPDATE ITERATIONS SET OPTIONS = :OPTIONS WHERE NUM = :NUM;';
C_UpdateRange = 'UPDATE ITERATIONS SET RANGEDATA = :RANGEDATA WHERE NUM = :NUM;';
C_UpdateMeas = 'UPDATE ITERATIONS SET MEASDATA = :MEASDATA WHERE NUM = :NUM;';
C_UpdateMeasCalc = 'UPDATE ITERATIONS SET MEASCALCDATA = :MEASCALCDATA WHERE NUM = :NUM;';
C_UpdateIterCalc = 'UPDATE ITERATIONS SET ITERCALCDATA = :ITERCALCDATA WHERE NUM = :NUM;';
C_UpdateResults = 'UPDATE ITERATIONS SET RESULTS = :RESULTS WHERE NUM = :NUM;';
C_UpdateNextOptions = 'UPDATE ITERATIONS SET NEXTOPTIONS = :NEXTOPTIONS WHERE NUM = :NUM;';
var
vQR : TEmbSQLRequest;
vInsert : Boolean;
vArgsIdx : Byte;
vNum : Word;
vMs : TMemoryStream;
vStr : TStringList;
begin
  Result := False;
  try
    vNum := ANum;
    vQR.Prepare(ADB.DB, c_SqlText);
    vQR.Bind(1, vNum);
    vInsert := (vQR.PrepareNext <> SQLEMB_DONE)
      or (vQR.Step <> SQLEMB_ROW);
    vQR.Close;
    if vInsert then
      Result := CreateRecord(ADB, vNum)
    else
      Result := true;

    if (not Result)
    or (Length(Args) < 1) then
      Exit;
    Result := False;
    vMs := TMemoryStream.Create;
    vStr := TStringList.Create;
    try
      ADB.TransactionBegin();
      try
        for vArgsIdx := Low(Args) to High(Args) do
        begin
          vStr.Clear;
          vMs.Clear;
          Args[vArgsIdx].SourceIni.GetStrings(vStr);
          vStr.SaveToStream(vMs);
          vMs.Position := 0;
          case Args[vArgsIdx].Option of
            foOptions: vQR.Prepare(ADB.DB, C_UpdateOptions);
            foRange: vQR.Prepare(ADB.DB, C_UpdateRange);
            foMeas:  vQR.Prepare(ADB.DB, C_UpdateMeas);
            foMeasCalc: vQR.Prepare(ADB.DB, C_UpdateMeasCalc);
            foIterCalc: vQR.Prepare(ADB.DB, C_UpdateIterCalc);
            foResults: vQR.Prepare(ADB.DB, C_UpdateResults);
            foNextOptions: vQR.Prepare(ADB.DB, C_UpdateNextOptions);
          end;
          vQR.Bind(1, vMS);
          vQR.Bind(2, vNum);
          vQR.Execute;
        end;
        ADB.Commit;
        Result := True;
      except
        Result := false;
        if ADB.TransactionActive then
          ADB.RollBack;
      end;
    finally
      FreeAndNil(vMs);
      FreeAndNil(vStr);
    end;
  except
    Result := false;
  end;
end;


{TMasIteration}

constructor TMasIteration<TCalc, TMeas>.Create(const ADBExt : TPositionDBExtention);
begin
  inherited Create(ADBExt);
  fMeasPoints := TIterPointsList.Create(Self);
  fOptions := TIterOptions.Create;
  fNextIterOptions := TIterOptions.Create;
  fErrors := TErrorsObj.Create;
  fMeasSolutions := TMeasures<TMeas>.Create(Self);
  fCalcResults := TSolutions<TCalc>.Create(Self);
end;

destructor TMasIteration<TCalc, TMeas>.Destroy;
begin
  FreeAndNil(fMeasSolutions);
  FreeAndNil(fCalcResults);
  FreeAndNil(fErrors);
  FreeAndNil(fNextIterOptions);
  FreeAndNil(fOptions);
  FreeAndNil(fMeasPoints);
  inherited Destroy;
end;

function TMasIteration<TCalc, TMeas>.Read(const ADB: TEmbSQLDataBase; ANum: Word): boolean;
var
vFormatSettings: TFormatSettings;
begin
  vFormatSettings:= TFormatSettings.Create(1033);
  result := Read(ADB, ANum, vFormatSettings);
end;

function TMasIteration<TCalc, TMeas>.Read(const ADB: TEmbSQLDataBase;
                                          ANum: Word;
                                          const AFormatSettings: TFormatSettings): boolean;
var
vFO : TFieldOption;
vIdx,
vMeasIdx : Byte;
vArgs : TIOArgs;
vMeasSol : TMeas;
vResMeasSol : TMeasure<TCalc>;
vCalcs : TSolutions<TCalc>;
begin
  result := inherited Read(ADB, ANum);
  if not Result then
    Exit;

  SetLength(vArgs, Ord(High(vFO))+1);
  for vFO := Low(vFO) to High(vFO) do
  begin
    vArgs[ord(vFO)].Option := vFO;
    vArgs[ord(vFO)].SourceIni := TMemIniFile.Create('');
  end;
  try
    Result := IORead(ADB, ANum, vArgs);
    for vIdx := Low(vArgs) to High(vArgs) do
    begin
      case vArgs[vIdx].Option of
        foOptions: fOptions.Read(vArgs[vIdx].SourceIni, AFormatSettings);
        foRange: fMeasPoints.Read(vArgs[vIdx].SourceIni, AFormatSettings);
        foMeas: fMeasSolutions.Read(vArgs[vIdx].SourceIni, 'Meas', AFormatSettings);
        foMeasCalc:
        begin
          vMeasIdx := 0;
          while vMeasIdx < fMeasSolutions.Count  do
          begin
            vMeasSol := fMeasSolutions[vMeasIdx];
            vCalcs := vMeasSol.CalcSolutions;
            vCalcs.Read(vArgs[vIdx].SourceIni, Format('MeasCalc%d',[vMeasIdx]), AFormatSettings);
            inc(vMeasIdx);
          end;
        end;
        foIterCalc: fCalcResults.Read(vArgs[vIdx].SourceIni, 'IterCalc', AFormatSettings);
        foResults:
        begin
          vMeasIdx := 0;
          while vMeasIdx < fMeasSolutions.Count  do
          begin
            vResMeasSol := fMeasSolutions[vMeasIdx];
            vResMeasSol.Errors.Read(vArgs[vIdx].SourceIni, Format('Meas%dErr',[vMeasIdx]));
            inc(vMeasIdx);
          end;
          fErrors.Read(vArgs[vIdx].SourceIni, 'IterErr');
          Results.Read(vArgs[vIdx].SourceIni, 'IterResults', AFormatSettings)
        end;
        foNextOptions: fNextIterOptions.Read(vArgs[vIdx].SourceIni, AFormatSettings);
      end;
    end;
  finally
    for vIdx := Low(vArgs) to High(vArgs) do
      FreeAndNil(vArgs[vIdx].SourceIni);
    SetLength(vArgs, 0);
  end;
end;

function TMasIteration<TCalc, TMeas>.Write(const ADB: TEmbSQLDataBase; AIterState: Boolean;
  AOptions: TFieldOptions): boolean;
var
vFO : TFieldOption;
vIdx,
vMeasIdx : Byte;
vArgs : TIOArgs;
vFormatSettings: TFormatSettings;
vMeasSol : TMeas;
vResMeasSol : TMeasure<TCalc>;
vCalcs : TSolutions<TCalc>;
begin
  Result := False;
  if AIterState then
  begin
    Result := WriteState(ADB, Num, State);
    if not Result then
      Exit;
  end;
  if AOptions = [] then
    Exit;
  Result := False;
  vFormatSettings := TFormatSettings.Create(1033);
  SetLength(vArgs, 0);
  for vFO := Low(vFO) to High(vFO) do
  begin
    if (vFO in AOptions) then
    begin
      SetLength(vArgs, Length(vArgs)+1);
      vArgs[High(vArgs)].Option := vFO;
      vArgs[High(vArgs)].SourceIni := TMemIniFile.Create('');
    end;
  end;
  try
    for vIdx := Low(vArgs) to High(vArgs) do
    begin
      case vArgs[vIdx].Option of
        foOptions: fOptions.Write(vArgs[vIdx].SourceIni, vFormatSettings);
        foRange: fMeasPoints.Write(vArgs[vIdx].SourceIni, vFormatSettings);
        foMeas: fMeasSolutions.Write(vArgs[vIdx].SourceIni, 'Meas', vFormatSettings);
        foMeasCalc:
        begin
          vMeasIdx := 0;
          while vMeasIdx < fMeasSolutions.Count  do
          begin
            vMeasSol := fMeasSolutions[vMeasIdx];
            vCalcs := vMeasSol.CalcSolutions;
            vCalcs.Write(vArgs[vIdx].SourceIni, Format('MeasCalc%d',[vMeasIdx]), vFormatSettings);
            inc(vMeasIdx);
          end;
        end;
        foIterCalc: fCalcResults.Write(vArgs[vIdx].SourceIni, 'IterCalc', vFormatSettings);
        foResults:
        begin
          vMeasIdx := 0;
          while vMeasIdx < fMeasSolutions.Count  do
          begin
            vResMeasSol := fMeasSolutions[vMeasIdx];
            vResMeasSol.Errors.Write(vArgs[vIdx].SourceIni, Format('Meas%dErr',[vMeasIdx]));
            inc(vMeasIdx);
          end;
          fErrors.Write(vArgs[vIdx].SourceIni, 'IterErr');
          Results.Write(vArgs[vIdx].SourceIni, 'IterResults', vFormatSettings)
        end;
        foNextOptions: fNextIterOptions.Write(vArgs[vIdx].SourceIni, vFormatSettings);
      end;
    end;
    Result := IOWrite(ADB, Num, vArgs);
  finally
    for vIdx := Low(vArgs) to High(vArgs) do
      FreeAndNil(vArgs[vIdx].SourceIni);
    SetLength(vArgs, 0);
  end;
end;

procedure TMasIteration<TCalc, TMeas>.MoveCalcToMeas;
var
vIdx : Word;
vNewMeasSol : TMeasure<TCalc>;
begin
  fMeasSolutions.Clear;
  for vIdx := 0 to fCalcResults.Count - 1 do
  begin
    vNewMeasSol := fMeasSolutions.NewItem;
    vNewMeasSol.Registers.Assign(fCalcResults[vIdx].Registers);
  end;
end;

{ TPointsList }

procedure TIterPointsList.CopyFrom(ASource: TList<TInPointOption>);
var
vIdx : Word;
vItem : TMasMeasPoint;
begin
  Clear;
  if ASource.Count < 1 then
    Exit;
  for vIdx := 0 to ASource.Count - 1 do
  begin
    vItem := TMasMeasPoint.Create;
    vItem.PatternID := ASource[vIdx].PatternID;
    vItem.PointValue := ASource[vIdx].PointValue;
    vItem.PatternExists := ASource[vIdx].PatternExists;
    vItem.MeasInf := ASource[vIdx].MeasInf;
    vItem.WorkPoint := ASource[vIdx].WorkPoint;
    vItem.MeasSucc.i := 0;
    Add(vItem);
  end;
end;

constructor TIterPointsList.Create(const AOwner: TMasIterationBase);
begin
  inherited Create(true);
  fIteration := AOwner;
end;

function TIterPointsList.Find(Value, Precis: double; var Index: Word): boolean;
begin
  Index := 0;
  while (Index < Count) do
  begin
    if SameValue(Items[Index].PointValue,Value, Precis) then
      Break;
    inc(Index);
  end;
  result := Index < Count;
end;

function TIterPointsList.PointExists(Value, Precis: double): boolean;
var
vFndIdx : word;
begin
  vFndIdx := 0;
  Result := Find(Value, Precis, vFndIdx);
end;

procedure TIterPointsList.Read(const ASource: TMemIniFile; const AFormatSettings: TFormatSettings);
var
vStr : TStringList;
vIdx : Integer;
vTmp : Double;
vItem : TMasMeasPoint;
begin
  Self.Clear;
  vStr := TStringList.Create;
  try
    ASource.ReadSections(vStr);
    if vStr.Count < 1 then
      Exit;
    for vIdx := 0 to vStr.Count-1 do
    begin
      if ASource.ValueExists(vStr[vIdx], 'Value')
      and ASource.ValueExists(vStr[vIdx], 'PatternID')
      and ASource.ValueExists(vStr[vIdx], 'PatternExists')
      and ASource.ValueExists(vStr[vIdx], 'MeasInf')
      and ASource.ValueExists(vStr[vIdx], 'WorkPoint')
      and ASource.ValueExists(vStr[vIdx], 'MeasSucc') then
      begin
        vTmp := ReadFloat(ASource, vStr[vIdx], 'Value', 25, AFormatSettings);
        if not PointExists(vTmp, 0.01) then
        begin
          vItem := TMasMeasPoint.Create;
          vItem.PointValue := vTmp;
          vItem.PatternID := ASource.ReadInteger(vStr[vIdx], 'PatternID', 0);
          vItem.PatternExists := ASource.ReadBool(vStr[vIdx], 'PatternExists', false);
          vItem.MeasInf :=  ASource.ReadBool(vStr[vIdx], 'MeasInf', false);
          vItem.WorkPoint := ASource.ReadBool(vStr[vIdx], 'WorkPoint', true);
          vItem.MeasSucc.i :=  ASource.ReadInteger(vStr[vIdx], 'MeasSucc', 0);
          Add(vItem);
        end;
      end;
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

function TIterPointsList.SuccCount: Word;
var
vIdx : Word;
vIter : TMasIteration<TSolution, TMeasure<TSolution>>;
begin
  vIdx := 0;
  Result := 0;
  vIter := TMasIteration<TSolution, TMeasure<TSolution>>(fIteration);
  while vIdx < Count do
  begin
    if (Items[vIdx].MeasSucc.i =
      vIter.fOptions.ControlOptions.InRangeOptions[vIter.fOptions.ControlOptions.MeasureMode].i) then
      inc(Result);
    inc(vIdx);
  end;
end;

function TIterPointsList.WorksCount: Word;
var
vIdx : Word;
begin
  vIdx := 0;
  Result := 0;
  while vIdx < Count do
  begin
    if (Items[vIdx].WorkPoint) then
      inc(Result);
    inc(vIdx);
  end;
end;

procedure TIterPointsList.Write(const ADest: TMemIniFile; const AFormatSettings: TFormatSettings);
var
vIdx : Byte;
begin
  vIdx := 0;
  while vIdx < Count do
  begin
    WriteFloat(ADest, Format('Point%d',[vIdx]), 'Value', Items[vIdx].PointValue, AFormatSettings);
    ADest.WriteInteger(Format('Point%d',[vIdx]), 'PatternID', Items[vIdx].PatternID);
    ADest.WriteBool(Format('Point%d',[vIdx]), 'PatternExists', Items[vIdx].PatternExists);
    ADest.WriteBool(Format('Point%d',[vIdx]), 'MeasInf', Items[vIdx].MeasInf);
    ADest.WriteBool(Format('Point%d',[vIdx]), 'WorkPoint', Items[vIdx].WorkPoint);
    ADest.WriteInteger(Format('Point%d',[vIdx]), 'MeasSucc', Items[vIdx].MeasSucc.i);
    inc(vIdx);
  end;
end;


{ TMedItem }

function TMedItem.Percents: double;
begin
  if TotalCount > 0 then
    result := ErrCount*100/TotalCount
  else
    result := 0;
end;

{ TErrorsObj }

procedure TErrorsObj.Clear;
begin
  while count > 0 do
    delete(count -1);
  inherited;
end;

function TErrorsObj.Find(ADescript: TMeasErrorDescript;
  out oItem: pMedItem): boolean;
var i : Word;
begin
  i := 0;
  result := false;
  oItem := nil;
  while (i < Count) and (Get(i)^.Descript <> ADescript) do
    inc(i);
  result := i < Count;
  if result then
     oItem := Items[i];
end;

function TErrorsObj.First: pMedItem;
begin
  result := Get(0);
end;

function TErrorsObj.Get(Index: Integer): pMedItem;
begin
  Result := pMedItem(inherited Get(Index));
end;

function TErrorsObj.Last: pMedItem;
begin
  result := Get(Count - 1);
end;

function TErrorsObj.NewItem(Descript: TMeasErrorDescript): pMedItem;
begin
  New(result);
  result^.Descript := Descript;
  result^.TotalCount := 0;
  result^.ErrCount := 0;
  inherited Add(result);
end;

procedure TErrorsObj.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
      Dispose(Ptr);
  inherited Notify(Ptr, Action);
end;

procedure TErrorsObj.Read(AIniFile: TMemIniFile; const ASecSuff : string);
var e : TMeasErrorDescript;
vItem : pMedItem;
vTotal, vError : word;
begin
  for e := low(e) to high(e) do
  begin
    vTotal := AIniFile.ReadInteger(Format('%s%s',[MeasErrorDescriptSection[e], ASecSuff]), 'TotalCount', 0);
    vError := AIniFile.ReadInteger(Format('%s%s',[MeasErrorDescriptSection[e], ASecSuff]), 'ErrCount', 0);
    if (vTotal < 1) or (vError < 1) then
      continue;
    if not Find(e, vItem) then
      vItem := NewItem(e);
    vItem^.TotalCount := vTotal;
    vItem^.ErrCount := vError;
  end;
end;

function TErrorsObj.TotalCount: word;
var e : TMeasErrorDescript;
vItem : pMedItem;
begin
  result := 0;
  for e := low(e) to high(e) do
    if Find(e, vItem) then
       inc(result, vItem^.TotalCount);
end;

function TErrorsObj.TotalErrorCount: word;
var
e : TMeasErrorDescript;
vItem : pMedItem;
begin
  result := 0;
  for e := low(e) to high(e) do
  begin
    if Find(e, vItem) then
       inc(result, vItem^.ErrCount);
  end;
end;

procedure TErrorsObj.Write(AIniFile: TMemIniFile; const ASecSuff : string);
var
e : TMeasErrorDescript;
vItem : pMedItem;
begin
  for e := low(e) to high(e) do
  begin
    if Find(e, vItem)
    and (vItem^.TotalCount > 0)
    and (vItem^.ErrCount > 0) then
    begin
      AIniFile.WriteInteger(Format('%s%s',[MeasErrorDescriptSection[e], ASecSuff]), 'TotalCount', vItem^.TotalCount);
      AIniFile.WriteInteger(Format('%s%s',[MeasErrorDescriptSection[e], ASecSuff]), 'ErrCount', vItem^.ErrCount);
    end;
  end;
end;

{ TCurrIterMeasureSolution }

constructor TMeasure<TCalc>.Create(const AIteration : TMasIterationBase);
begin
  inherited Create(AIteration);
  fCalcSolutions := TSolutions<TCalc>.Create(AIteration);
  fErrors := TErrorsObj.Create;
end;

destructor TMeasure<TCalc>.Destroy;
begin
  fCalcSolutions.Clear;
  fErrors.Clear;
  FreeAndNil(fCalcSolutions);
  FreeAndNil(fErrors);
  inherited Destroy;
end;

function TMeasure<TCalc>.NewCalc: TCalc;
var
vSelfIdx : Byte;
vIter : TMasIteration<TCalc, TMeasure<TCalc>>;
begin
  vIter := TMasIteration<TCalc, TMeasure<TCalc>>(Iteration);
  vSelfIdx := Byte(vIter.MeasSolutions.IndexOf(self));
  result := TCalc.Create(Iteration, vSelfIdx);
  CalcSolutions.Add(result);
end;

procedure TMeasure<TCalc>.Read(const ASource: TMemIniFile; const ASection: string;
  const AFormatSettings: TFormatSettings);
begin
  inherited;
  fErrors.Read(ASource, ASection);
end;

procedure TMeasure<TCalc>.Write(const ADest: TMemIniFile; const ASection: string;
  const AFormatSettings: TFormatSettings);
begin
  inherited;
  fErrors.Write(ADest, ASection);
end;



{ TMasRegisters }

procedure TMasIterationRegisters.AfterCreate;
begin
end;

constructor TMasIterationRegisters.Create(const AIteration: TMasIterationBase);
begin
  inherited Create;
  fRegisters := TMas6279D8Registers.Create(nil);
  fIteration := AIteration;
  Results.dF := 1E6;
  Results.dF25 := 1E6;
  AfterCreate;
end;

destructor TMasIterationRegisters.Destroy;
begin
  FreeAndNil(fRegisters);
  inherited;
end;

procedure TMasIterationRegisters.Read(const ASource: TMemIniFile; const ASection: string;
  const AFormatSettings: TFormatSettings);
begin
  fRegisters.LoadFromIni(ASource, ASection);
  Results.dF:= ReadFloat(ASource, ASection, 'dF', 1E6, AFormatSettings);
  Results.dF25 := ReadFloat(ASource, ASection, 'dF25', 1E6, AFormatSettings);
end;

procedure TMasIterationRegisters.Write(const ADest: TMemIniFile; const ASection: string; const AFormatSettings: TFormatSettings);
begin
  fRegisters.SaveToIni(ADest, ASection);
  WriteFloat(ADest, ASection, 'dF', Results.dF, AFormatSettings);
  WriteFloat(ADest, ASection, 'dF25', Results.dF25, AFormatSettings);
end;

{ TCalcSolution }

constructor TSolution.Create(const AIteration: TMasIterationBase; const ARootIdx : Byte);
begin
  inherited Create(AIteration);
  fRootIdx := ARootIdx;
end;

(*procedure TSolution.Read(const ASource: TMemIniFile; const ASection: string;
  const AFormatSettings: TFormatSettings);
begin
  inherited;
  //RootIdx := Byte(ASource.ReadInteger(ASection, 'RootIdx', 0));
  {CalcResults.dF:= ReadFloat(ASource, ASection, 'dF', 1E8, AFormatSettings);
  CalcResults.dF25 := ReadFloat(ASource, ASection, 'dF25', 1E8, AFormatSettings);
  CalcResults.dReg := ReadFloat(ASource, ASection, 'dReg', 1E8, AFormatSettings);
  CalcResults.Prior := ReadFloat(ASource, ASection, 'Prior', 1E8, AFormatSettings); }
end; *)

procedure TSolution.Write(const ADest: TMemIniFile; const ASection: string;
  const AFormatSettings: TFormatSettings);
begin
  inherited;
  ADest.WriteInteger(ASection, 'RootIdx', RootIdx);
  {WriteFloat(ADest, ASection, 'dF', CalcResults.dF, AFormatSettings);
  WriteFloat(ADest, ASection, 'dF25', CalcResults.dF25, AFormatSettings);
  WriteFloat(ADest, ASection, 'dReg', CalcResults.dReg, AFormatSettings);
  WriteFloat(ADest, ASection, 'Prior', CalcResults.Prior, AFormatSettings);}
end;

{ TMasMeasPoint }

procedure TMasMeasPoint.MeasSuccInclude(AOption: TInRangeProcOption);
var
vTmp : TInRangeProcOptionSet;
begin
  vTmp := MeasSucc.s;
  Include(vTmp, AOption);
  MeasSucc.s := vTmp;
end;

{ TCalcList<T> }

constructor TMeasures<T>.Create(const AIteration: TMasIterationBase);
begin
  inherited Create(true);
  fIteration := AIteration;
end;

constructor TMeasures<T>.Create(const AIteration: TMasIterationBase;
  const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer, AOwnsObjects);
  fIteration := AIteration;
end;

constructor TMeasures<T>.Create(const AIteration: TMasIterationBase;
  const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(Collection, AOwnsObjects);
  fIteration := AIteration;
end;

function TMeasures<T>.GetItem(AIndex: Word): T;
begin
  Result := T(inherited items[AIndex]);
end;

function TMeasures<T>.NewItem: T;
begin
  result := T.Create(fIteration);
  Add(result);
end;

function TMeasures<T>.NewItem(AIndex: Word): T;
begin
  result := T.Create(fIteration);
  Insert(AIndex, result);
end;

procedure TMeasures<T>.Read(const ASource: TMemIniFile; const ASectionPreff: string;
  const AFormatSettings: TFormatSettings);
var
vStr : TStringList;
vIdx,
vFndIdx : Integer;
begin
  vStr := TStringList.Create;
  try
    Self.Clear;
    ASource.ReadSections(vStr);
    if vStr.Count < 1 then
      Exit;
    for vIdx := 0 to vStr.Count -1 do
    begin
      if vStr.Find(Format('%s%d',[ASectionPreff, vIdx]), vFndIdx) then
        NewItem.Read(ASource, vStr[vFndIdx], AFormatSettings);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

procedure TMeasures<T>.Write(const ADest: TMemIniFile; const ASectionPreff: string;
  const AFormatSettings: TFormatSettings);
var
vIdx : Byte;
begin
  vIdx := 0;
  while vIdx < Count do
  begin
    Items[vIdx].Write(ADest, Format('%s%d',[ASectionPreff, vIdx]), AFormatSettings);
    Inc(vIdx);
  end;
end;

procedure TRegistersDouble.Assign(const ASource: TRegistersDouble);
var
i : byte;
vItem : TRegisterValueDouble;
begin
  Self.Clear;
  if ASource.Count < 1 then
    Exit;
  for I := 0 to ASource.Count -1 do
  begin
    vItem.BitIndex := ASource[i].BitIndex;
    vItem.Value := ASource[i].Value;
    Add(vItem);
  end;
end;

function TRegistersDouble.IndexOfBit(ABitIndex: byte): Integer;
begin
  Result := 0;
  while Result < Count do
  begin
    if (Items[result].BitIndex = ABitIndex) then
      Break;
    inc(Result);
  end;
  if Result >= Count then
    Result := -1;
end;

procedure TRegistersDouble.Read(const ASource: TMemIniFile;
  const ASection: string; const AFormatSettings: TFormatSettings);
var
vIdx : byte;
vItem : TRegisterValueDouble;
begin
  Self.Clear;
  if not ASource.SectionExists(ASection) then
    Exit;
  for vIdx := C_BitMinIndex to C_BitMaxIndex -1 do
  begin
    if ASource.ValueExists(ASection, C_RegNames[vIdx]) then
    begin
      vItem.BitIndex := vIdx;
      vItem.Value := ReadFloat(ASource, ASection, C_RegNames[vIdx], 1, AFormatSettings);
      Add(vItem);
    end;
  end;
end;

procedure TRegistersDouble.Read(const ASource: TMemIniFile; const AFormatSettings: TFormatSettings);
var
vIdx : byte;
vItem : TRegisterValueDouble;
begin
  Self.Clear;
  if not ASource.SectionExists('WeightRegisters') then
    Exit;
  for vIdx := C_BitMinIndex to C_BitMaxIndex -1 do
  begin
    if ASource.ValueExists('WeightRegisters', C_RegNames[vIdx]) then
    begin
      vItem.BitIndex := vIdx;
      vItem.Value := ReadFloat(ASource, 'WeightRegisters', C_RegNames[vIdx], 1, AFormatSettings);
      Add(vItem);
    end;
  end;
end;

procedure TRegistersDouble.Write(const ADest: TMemIniFile; const AFormatSettings: TFormatSettings);
var
vIdx : Byte;
begin
  ADest.EraseSection('WeightRegisters');
  vIdx := 0;
  while vIdx < Count do
  begin
    WriteFloat(ADest, 'WeightRegisters', C_RegNames[Items[vIdx].BitIndex], Items[vIdx].Value, AFormatSettings);
    Inc(vIdx);
  end;
end;

{ TSolutions<T, TRoot> }

function TSolutions<T>.NewItem(const ARootIdx : Byte): T;
begin
  Result := T.Create(Iteration, ARootIdx);
  inherited Add(Result);
end;

function TSolutions<T>.NewItem(const ARootIdx : Byte; AIndex: Word): T;
begin
  result := T.Create(Iteration, ARootIdx);
  Insert(AIndex, result);
end;

procedure TSolutions<T>.Read(const ASource: TMemIniFile; const ASectionPreff: string;
  const AFormatSettings: TFormatSettings);
var
vStr : TStringList;
vIdx,
vFndIdx,
vRootIdx : Integer;
vIter : TMasIteration<TSolution, TMeasure<TSolution>>;
begin
  vIter := TMasIteration<TSolution, TMeasure<TSolution>>(fIteration);
  vStr := TStringList.Create;
  try
    Self.Clear;
    ASource.ReadSections(vStr);
    if vStr.Count < 1 then
      Exit;
    for vIdx := 0 to vStr.Count -1 do
    begin
      if vStr.Find(Format('%s%d',[ASectionPreff, vIdx]), vFndIdx)
      and ASource.ValueExists(vStr[vFndIdx], 'RootIdx') then
      begin
        vRootIdx := ASource.ReadInteger(vStr[vFndIdx], 'RootIdx', 0);
        if (vRootIdx > -1)
        and (vRootIdx < vIter.MeasSolutions.Count) then
           NewItem(Byte(vRootIdx)).Read(ASource, vStr[vFndIdx], AFormatSettings);
      end;
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

end.
