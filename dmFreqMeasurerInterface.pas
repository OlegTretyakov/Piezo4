unit dmFreqMeasurerInterface;

interface

type 
  pFreqResult = ^TFreqResult;
  TFreqResult = record
    Freq,
    DispersionHz,
    DispersionPpm : double;
    TimeStamp : TDateTime;
    SeriesCount,
    SeriesSuccesCount: Byte;
    procedure Assign(ASource : pFreqResult);overload;
    procedure Assign(ASource : TFreqResult);overload;
  end;
  IdmFreqStarter = interface(IInterface)
 ['{77D9A6D0-E8BB-43FF-AE51-B7BBC09DE0EB}']
    function StartOnceMeasure: Boolean; overload; stdcall;
    function StartOnceMeasure(ATime{mSec*100}:Word): Boolean; overload; stdcall;
    function StartSeriesMeasure: Boolean; overload; stdcall;
    function StartSeriesMeasure(ATime{mSec*100}:Word; AQuant : Byte): Boolean; overload; stdcall;  
    function StartPermanentlyMeasure: Boolean; overload; stdcall;
    function StartPermanentlyMeasure(ATime{mSec*100}:Word): Boolean; overload; stdcall;   
    procedure StopPermanentlyMeasure; stdcall;
    function CalculatedTimeOut : Cardinal;stdcall;
  end;
  IdmFreqController = interface(IInterface)
  ['{DCF3D3BA-6D95-4823-9CAF-EE8D7A968CA5}']
    function GetSeriesCount: Byte; stdcall;
    function GetSeriesSuccesCount: Byte; stdcall;
  end;
  {Starting params}
  TFreqMeasParams = record
    ATime{mSec*100}: Word;
    AQuant : Byte;
  end;
  pFreqMeasParams = ^TFreqMeasParams;
  
  IdmFreqResults = interface(IInterface)
    ['{5FF93CB7-A422-42A3-9C2A-FA30FA62FD01}']
    function FreqResult(AIndex : Word; const AResult : pFreqResult):Boolean; stdcall;
  end;
  
  IdmPositionFreqResult = interface(IInterface)
   ['{575D6B8E-3958-42CD-A8D9-CE2327A54A1B}']
    function FreqResult(const AResult : pFreqResult):Boolean; stdcall;
  end;


implementation

{ TFreqResult }

procedure TFreqResult.Assign(ASource: pFreqResult);
begin
  Freq          := ASource^.Freq;
  DispersionHz  := ASource^.DispersionHz;
  DispersionPpm := ASource^.DispersionPpm;
  TimeStamp     := ASource^.TimeStamp;
end;

procedure TFreqResult.Assign(ASource: TFreqResult);
begin  
  Freq          := ASource.Freq;
  DispersionHz  := ASource.DispersionHz;
  DispersionPpm := ASource.DispersionPpm; 
  TimeStamp     := ASource.TimeStamp;
end;

end.
