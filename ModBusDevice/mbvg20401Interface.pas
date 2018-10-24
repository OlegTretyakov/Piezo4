unit mbvg20401Interface;

interface
  uses dmChannelsInterface;
  type
  Imbvg20401Module = interface(IdmChannels)
    ['{C91E872D-CBE9-4000-B66A-3CF0F6AE1FA8}']
    function GetVoltage(AIndex : Word):Double; stdcall;
    function GetTimeStamp(AIndex : Word):TDateTime; stdcall;
    property Voltage[AIndex : Word] : Double read GetVoltage;
    property TimeStamp[AIndex : Word] : TDateTime read GetTimeStamp;
  end;
implementation

end.
