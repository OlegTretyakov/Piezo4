unit MilandrRev8.MMFinished;

interface
  uses AbstractStpMethod;
  type

  Tmasd8MeasureFinishedThread = class(TStpThread)
   protected
    procedure Execute;override;
  end;

  Tmasd8MeasureFinished = class(TAbstractStpMethod)
   protected
    function ThreadClass : TStpThreadClass;override;
   public
    function ModuleName : string; override;
    function ReadyToStart:Boolean; override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

{ Tmasd8MeasureFinished }

function Tmasd8MeasureFinished.ModuleName: string;
begin
  result := 'Mas 6279D8 results processor';
end;

function Tmasd8MeasureFinished.ReadyToStart: Boolean;
begin

end;

procedure Tmasd8MeasureFinished.Start;
begin
  inherited;

end;

procedure Tmasd8MeasureFinished.Stop;
begin
  inherited;

end;

function Tmasd8MeasureFinished.ThreadClass: TStpThreadClass;
begin
  result := Tmasd8MeasureFinishedThread;
end;

{ Tmasd8MeasureFinishedThread }

procedure Tmasd8MeasureFinishedThread.Execute;
begin


end;

end.
