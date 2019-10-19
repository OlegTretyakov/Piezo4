unit AbstractProtocol;

interface
  uses
    System.Classes, AbstractExtention;

  type
   TSysInfoStruct = record
     ProtocolVersion,
     ModelCode,
     HwVer,
     FwVer,
     Serial : Word;
     MaxPositionsCount, LoadedModulesCount : Byte;
     ProtocolDestription,
     LoadedModules : string;
   end;

   pSysInfoStruct = ^TSysInfoStruct;

   TAbstractProtocol = class;

   TProtocolEvent = procedure (Sender : TAbstractProtocol; const AStr : string) of object;
   TProtocolPacket = procedure (Sender : TAbstractProtocol; const AStr : string) of object;

   TAbstractProtocol = class (TAbstractExtention)
    private
     fOnProtocolMessage : TProtocolEvent;
     fOnProtocolError : TProtocolEvent;
     fProtocolPacket : TProtocolPacket;
    public
     class function ProtocolName : string; virtual; abstract;
     function CommChannel : string; virtual; abstract;
     function Connected : Boolean; virtual; abstract;
     procedure FillSysInfo(SysInfoStruct : pSysInfoStruct); virtual;
     procedure SaveDBConfig(const ADest : TStrings); virtual;
     procedure FillBoardExtentionsClasses(var oClasses :TExtentionsClasses); virtual;
     procedure FillPositionsListExtentionsClasses(var oClasses :TExtentionsClasses); virtual;
     procedure Close; virtual;
     property OnProtocolMessage : TProtocolEvent read fOnProtocolMessage write fOnProtocolMessage;
     property OnProtocolError : TProtocolEvent read fOnProtocolError write fOnProtocolError;
     property ProtocolPacket : TProtocolPacket read fProtocolPacket write fProtocolPacket;
   end;
   TAbstractProtocolClass = class of TAbstractProtocol;
   
implementation

{ TAbstractProtocol }

procedure TAbstractProtocol.Close;
begin
end;

procedure TAbstractProtocol.FillBoardExtentionsClasses(var oClasses: TExtentionsClasses);
begin
  SetLength(oClasses, 0);
end;

procedure TAbstractProtocol.FillPositionsListExtentionsClasses(var oClasses: TExtentionsClasses);
begin 
  SetLength(oClasses, 0);
end;

procedure TAbstractProtocol.FillSysInfo(SysInfoStruct: pSysInfoStruct);
begin
  SysInfoStruct.ProtocolDestription := 'N\A';
  SysInfoStruct.LoadedModules := '';
  SysInfoStruct.ProtocolVersion := 0;
  SysInfoStruct.ModelCode := 0;
  SysInfoStruct.HwVer := 0;
  SysInfoStruct.FwVer := 0;
  SysInfoStruct.Serial := 0;
  SysInfoStruct.MaxPositionsCount := 0;
  SysInfoStruct.LoadedModulesCount := 0;
end;

procedure TAbstractProtocol.SaveDBConfig(const ADest: TStrings);
begin
end;

end.
