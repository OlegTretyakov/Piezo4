unit ProtocolTypes;

interface
  uses
    System.Classes,
    commtypes,
    AbstractTag;

type
  TProtocolErrorEvent = procedure (Result : TProtocolIOResult; APacket : pIOPacket) of object;
  //procedure called by the scan read thread to read data from your device.
  TScanReadProc = procedure (var oRescanAfter : Cardinal) of object;
  //function called by the scan write thread to write data on your device.
  TSafeIOFunct = function(const TagObj : TAbstractTag): TProtocolIOResult of object;


  IProtocolTag = interface(IInterface)
    ['{9217E70F-82A6-481E-A8F9-92D617D3ECD2}']
    procedure SetDataChanged(ADataChanged : Boolean);
  end;

  IProtocolPLCTag = interface(IProtocolTag)
    ['{7AB319AC-6519-4FB5-B7E3-6B772558268B}']
    procedure SetLastError(AError : TProtocolIOResult);
    procedure CallAfterRead;
    procedure WriteOK;
  end;

  IProtocolNotifications = interface(IInterface)
    ['{6C30C567-1FAE-4DF6-B665-52D98CD42B99}']
    function GetFireAsync : Boolean;
    //: Notifies when a successful read occurs.
    procedure NotifyReadOk;
    //: Notifies when a read fault occurs.
    procedure NotifyReadFault;
    //: Notifies when a successful write occurs.
    procedure NotifyWriteOk;
    //: Notifies when a write fault occurs.
    procedure NotifyWriteFault;
    property FireAsync : Boolean read GetFireAsync;
  end;

  IProtocolDiscreteTag = interface(IProtocolPLCTag)
    ['{383E3341-1B0B-4383-B19F-E91479CE37F1}']
    function GetReadedValue(AIndex: Word): boolean;
    procedure SetReadedValue(AIndex: Word; const Value: boolean);
    function GetToWriteValue(AIndex: Word): boolean;
    procedure SetToWriteValue(AIndex: Word; const Value: boolean);
    property ValuesReaded[AIndex : Word]:boolean read GetReadedValue write SetReadedValue;
    property ValuesToWrite[AIndex : Word]:boolean read GetToWriteValue write SetToWriteValue;
    property Values[AIndex : Word]:Boolean read GetReadedValue write SetToWriteValue;
  end;

  IProtocolAnalogTag = interface(IProtocolPLCTag)
   ['{0BBB9BAA-8AD6-446B-ADE9-2B393E66070B}']
    function GetReadedValue(AIndex: Word): Word;
    procedure SetReadedValue(AIndex: Word; const Value: Word);
    function GetToWriteValue(AIndex: Word): Word;
    procedure SetToWriteValue(AIndex: Word; const Value: Word);
    property ValuesReaded[AIndex : Word]:Word read GetReadedValue write SetReadedValue;
    property ValuesToWrite[AIndex : Word]:Word read GetToWriteValue write SetToWriteValue;
    property Values[AIndex : Word]:Word read GetReadedValue write SetToWriteValue;
  end;


const
  //: Identifies a message that does a scan read.
  PSM_TAGSCANREAD = 205;
  //: Identifies a message that does a scan write.
  PSM_TAGSCANWRITE =  206;
  //: Identifies a message that does a fire protocol error event.
  PSM_PROTOCOL_ERROR = 207;


implementation

end.
 
