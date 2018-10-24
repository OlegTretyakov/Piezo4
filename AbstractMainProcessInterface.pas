unit AbstractMainProcessInterface;

interface
  uses System.Classes, System.Generics.Collections, System.IniFiles, LoggerInterface;
  type  
  THwTestResult = (trChamberOK);
  THwTestResults = set of THwTestResult;
  IMainProcess = interface(IInterface)
   ['{30CC0C73-9A3F-49EE-8379-6015BD00A7FE}']
    function GetHwTestResult: THwTestResults; stdcall;
    procedure Log(ALogLevel : TLogInfo; const AMessage : string);stdcall;
    procedure DoInfoMessage(const AMessage : string; ALevel: byte=0);stdcall;
    procedure SendStartHardwareTest;stdcall;
    property HwTestResult : THwTestResults read GetHwTestResult;
  end;

  {$IFDEF DEBUG}
   IMainProcessDebug = interface(IMainProcess)
   ['{313263C2-0E5B-4E13-98E7-7D62AD016CDA}']
    procedure PostMessage(Msg : Cardinal; WParam, LParam : Integer); stdcall;
   end;
  {$ENDIF}
  IMainProcessAppTitle = interface(IInterface)
    ['{9C83DFEB-623A-4172-8B81-17A1AE4292E9}']
    function AppTitle : String; stdcall;
  end;
  IMainProcessStartStopController = interface(IInterface)
    ['{379CC5BF-802B-4179-AE83-81BE440B9574}']
    procedure Start; stdcall;
    procedure Stop; stdcall;
  end;
  IMainProcessRestartController = interface(IInterface)
    ['{07EAEE7E-D372-484B-BC93-AFD4CAF909C7}']
    procedure Restart; stdcall;
  end;
  IChamberPortGetter = interface(IInterface)
    ['{517873A3-F957-4BC4-A993-2029F0B473FA}'] 
    function Port: Byte; stdcall;
  end;
  ILoopedControl = interface(IInterface)
    ['{F6A8FD25-CCDE-4CE5-A3F1-6FDDE13AE43B}']
    procedure SetLooped(const Value: boolean); stdcall;
    function GetLooped : Boolean; stdcall;
    property Looped : boolean read GetLooped write SetLooped;
  end;
  IMainProcessState = interface(IInterface)
    ['{C733D2A6-63B2-472A-A8E8-9A811AE0333E}']
    function GetWorked : Boolean; stdcall;
    property Worked : Boolean read GetWorked;
  end;
  IAbstractProcessLogger = interface(IInterface)
    ['{282CAF09-2C0B-4072-9C86-35CDC07F0785}']  
    procedure AchLogs(AOwner : TComponent);stdcall;
    function ShowLogFrm(AOwner : TComponent):TComponent; stdcall;
    procedure ShowModalLogFrm(AOwner : TComponent);stdcall;
  end;
  IConfigController = interface(IInterface)
    ['{7348402A-B08E-4485-A978-BFFC64A377CC}']
    procedure Load(const AType: string; ADest: TMemoryStream);overload; stdcall;
    procedure Load(const AType: string; ADest: TStrings);overload; stdcall;
    procedure Load(const AType: string; ADest: TMemIniFile);overload; stdcall;
    function Save(const AType: string; ASource: TStream): boolean; overload; stdcall;
    function Save(const AType: string; ASource: TStrings): boolean; overload; stdcall;
    function Save(const AType: string; ASource: TMemIniFile): boolean; overload; stdcall;
    function Delete(const AType: string):boolean; stdcall;
  end;
  TBoardDescript = class(TObject)
   private
    fIniFile : TMemIniFile;
   public
    Serial,
    PositionsCount,
    CurrCount : Word;
    InstalledGUID : TGUID;
    constructor Create;
    destructor Destroy; override;
    property IniFile : TMemIniFile read fIniFile;
  end;
  TDBBoards = class(TObjectList<TBoardDescript>)
   function NewItem : TBoardDescript;
   function IndexOfSerial(ASerail : Word):Integer;
  end;
  IDBBoards = interface(IInterface)
    ['{079D9E3D-6E27-4EA3-A42A-F2724277FE6C}']
    procedure FillBoardsList(AList : TDBBoards); stdcall;
    function DeleteBoardRecord(ABoardSerial: Word): Boolean; stdcall;
  end;
  TInfoMessage = record
    Level: byte;
    Msg : String;
  end; 
  pInfoMessage = ^TInfoMessage;
 
const
C_OnDBConnected :TGUID = '{535D78DB-7704-4619-A7C9-AB0585958FFB}';
C_OnInfoMessage :TGUID = '{2EBF20B4-4B63-4059-B25D-325AAD3A94EC}';
C_OnHWTestOK : TGUID = '{65981E7A-7265-46BF-B7C9-5F5D7482C22F}';
C_OnHWTestFail : TGUID = '{26748EA7-2E6F-429E-BC55-D4580746CD9E}';


implementation

uses
System.SysUtils;

{ TBoardDescript }

const
C_Default_CHIP : TGUID = '{00000000-0000-0000-0000-000000000000}';

constructor TBoardDescript.Create;
begin
  fIniFile := TMemIniFile.Create('');
  InstalledGUID := C_Default_CHIP;
end;

destructor TBoardDescript.Destroy;
begin
  FreeAndNil(fIniFile);
  inherited;
end;

{ TDBBoards }

function TDBBoards.IndexOfSerial(ASerail: Word): Integer;
begin
  Result := 0;
  while Result < Count do
  begin
    if (Items[result].Serial = ASerail)  then
      Break;
    inc(Result);
  end;
  if Result >= Count then
    Result := -1;
end;

function TDBBoards.NewItem: TBoardDescript;
begin
  Result := TBoardDescript.Create;
  Add(Result);
end;

end.
