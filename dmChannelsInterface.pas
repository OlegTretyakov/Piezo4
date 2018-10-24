unit dmChannelsInterface;

interface
  type
  IdmChannels = interface(IInterface)
    ['{4A056EF1-9EF3-42C4-8A6F-77121357E1ED}']
    function GetChannelsCount : word; stdcall;
    property Count : word read GetChannelsCount;
  end;
implementation

end.
