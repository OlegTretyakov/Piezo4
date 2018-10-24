unit CalcLogger;

interface
   type

  ICalcLoggerGetter = interface(IInterface)
    ['{CBBDAE0A-BEE3-4EA3-97BF-F8731C53382D}']
    function Get(out obj):Boolean; stdcall;
  end;
implementation

end.
