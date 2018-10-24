unit ChamberInterfaces;

interface
uses
{$IFDEF  VER320+}
System.Classes, Winapi.Windows;
{$ELSE}
Classes, Windows, Messages;
{$ENDIF}
  type
  TTermoPoint = record
  private
   fTempr,
   fSpeed : double;
   fExpos : Byte;
   procedure SetTempr(const Value: double);
   procedure SetSpeed(const Value: double);
  public
   property Tempr : double read fTempr write SetTempr;
   property Speed : double read fSpeed write SetSpeed;
   property Expos : Byte read fExpos write fExpos;
  end;

  PTermoPoint = ^TTermoPoint;


  TChamberStabMode = (mChallenge, mCharge, mInRange,
                      mInRangeTime, mWatch, mChargeTimeOut);
  IChamberProcess = interface (IInterface)
  ['{20B87F24-DCF5-464B-92D9-33AB40A3835E}'] 
    procedure SetProcessWorked(const Value: boolean); stdcall;
    function GetProcessWorked: boolean;stdcall;
    function GetTargetTempr : double;stdcall;
    function GetCurrTempr : double; stdcall;  
    function GetCurrPointTempr : double; stdcall;
    function GetPointsCount : Word; stdcall;
    function GetCurrentPointIdx: integer; stdcall;
    property PointsCount : Word read GetPointsCount;
    property CurrentPoint : integer read GetCurrentPointIdx;
    function FirstPoint : boolean; stdcall;
    function BOF: boolean;stdcall;
    function EOF : boolean; stdcall;
    function NextPoint : boolean;stdcall;
    function PrevPoint : boolean;stdcall;  
    function GetStabMode: TChamberStabMode; stdcall;
    property StabMode : TChamberStabMode read GetStabMode;
    property TargetTempr : double read GetTargetTempr;
    property CurrTempr : double read GetCurrTempr;
    property CurrPointTempr : double read GetCurrPointTempr; 
    procedure ProcessCurrent;stdcall;
    function MaxDeltaTempr : double; stdcall;
    procedure StartExposure(ASeconds : Cardinal);stdcall;
    function GetIsInExposure: boolean; stdcall;  
    property isExposure : boolean read GetIsInExposure;
    procedure StopExposure; stdcall;
    function ExposureSecondElapsed: Cardinal;stdcall;
    function GetStartingTime: TDateTime; stdcall; 
    function IsExposureDone : boolean; stdcall;
    procedure ManualChargeTempr(const Tempr : double; const Minutes : Cardinal = 0); stdcall;
    procedure ManualChargeTemprBySpeed(const Target, SpeedDegMin: double);stdcall;
    procedure ManualTemperatureOff; stdcall;
    procedure ManualChamberOff; stdcall; 
    function StartChamberModule(sPort : byte) : boolean;stdcall;
    procedure ClearTermoProfile;stdcall;
    function DefaultSpeed : double; stdcall;
    property ProcessWorked : boolean read GetProcessWorked write SetProcessWorked;
    procedure ShowForm(AOwner : TComponent); stdcall;
    procedure BlockForm(ADoBlock : boolean); stdcall;
    procedure CloseForm; stdcall;  
    procedure SendDisconnectAndWait;stdcall;
  end;
  IChamberConnectionState = interface(IInterface)
    ['{AC4CAD03-C0EA-417E-91B4-A33FAE055082}']
    function GetConnected:Boolean; stdcall;
    function GetPort:Byte;stdcall;
    property Connected : Boolean read GetConnected;
    property Port : Byte read GetPort;
  end;
  IChamber = interface(IInterface)
    ['{AAE03D98-E554-43C8-9A24-2D5AEA4D0BD7}']
    function GetTargetTempr : double; stdcall;
    function GetCurrTempr : double; stdcall;     
    function GetSpeed: Double; stdcall;
    function GetSecondsTargetTime: Cardinal; stdcall;
    function GetSecondsTargetTimeOut: Cardinal;stdcall;
    function GetStabMode: TChamberStabMode; stdcall;
    procedure SetTargetTempr(const AValue : double); stdcall;
    property TargetTempr : double read GetTargetTempr write SetTargetTempr;
    property CurrTempr : double read GetCurrTempr;  
    property Speed: Double read GetSpeed;
    property SecondsTargetTime : Cardinal read GetSecondsTargetTime;
    property SecondsTargetTimeOut : Cardinal read GetSecondsTargetTimeOut;
    property StabMode : TChamberStabMode read GetStabMode;
  end;
  ITermoProfile = interface(IInterface)
  ['{6E30A950-67FC-407B-AB43-A5D21C193134}']
    function Count : Word; stdcall;
    function Get(Index: Integer): pTermoPoint; stdcall;
    function NewItem : pTermoPoint; stdcall;
    property Items[Index: Integer]: pTermoPoint read Get; default;
    function GetPrevTempr: double;stdcall;
    procedure SetPrevTempr(const ATempr : double); stdcall;
    property PrevTempr : double read GetPrevTempr write SetPrevTempr;
    function PointExists(const ATempr : double):boolean;stdcall;
    function IndexOf(const ATempr : double):integer;stdcall;
    procedure Sort; stdcall;
    procedure Clear; stdcall;
  end;

  TChamberIni = record
    DeltaTempr : double;
    DeltaTime,
    TimeOutCoeff : Cardinal;
    procedure LoadIni;
  end;
  const
  C_OnChamber_Detected :TGUID = '{D3291F13-8174-4166-A1EB-4463294605B0}';
  C_OnChamber_TryConnectionFail :TGUID = '{FC1A73B4-0F30-4483-AE4B-8B199EAD1147}';
  C_OnChamber_ConnectionLost  :TGUID = '{C8C13FAF-6F66-4A4A-B5E7-A425D23DE509}';
  C_OnChamber_Disconnected  :TGUID = '{55980E81-A66E-4A86-A20D-5C4CE4FF0BFA}';
  C_OnChamber_Tempr :TGUID = '{94CCD542-A235-4975-A7C4-D37A1BF4413A}';
  C_OnChamber_TemprStabilized :TGUID = '{A6114C2E-AF32-4324-903F-812A991A4DE7}';
  C_OnTemprStabilizedAndExposure : TGUID = '{3CD6D8D1-DBFF-4643-AAC2-F4C885955318}';
  C_OnChamber_StabilizedTimeOut :TGUID = '{9C27247D-4024-4FF8-A914-B462FA8E1F33}';
  C_OnChamber_TemprOutOfTarget :TGUID = '{9B3DCB8B-9F9E-4147-9D26-E164887656EE}';
  C_OnTempr_Charge :TGUID = '{ED005438-83CD-4848-B100-6A3F04571A1A}';
  C_OnProfileEnded:TGUID ='{0AC881B0-2C30-46C5-B76B-74FFE35A908A}';


implementation
uses
{$IFDEF  VER320+}
System.IniFiles,
Vodopad.CustomIniHelpers,
System.SysUtils,
System.Math;
{$ELSE}
IniFiles,
CustomIniHelpers,
SysUtils,
Math;
{$ENDIF}

{ TTermoPoint }

procedure TTermoPoint.SetSpeed(const Value: double);
begin
  fSpeed := EnsureRange(value, 0.1, 15);
end;

procedure TTermoPoint.SetTempr(const Value: double);
begin
  fTempr := EnsureRange(value, -75, 125);
end;

{ TChamberIni }

procedure TChamberIni.LoadIni;
var
f:TIniFile;
vPMLocalFormatSettings : TFormatSettings;
begin
  {$IFDEF  VER320+}
  vPMLocalFormatSettings := TFormatSettings.Create(1033);  
  {$ELSE}
  GetLocaleFormatSettings(1033, vPMLocalFormatSettings);
  {$ENDIF}
  f := TIniFile.Create(ChangeFileExt(GetModuleName(HInstance), '.ini'));
  try
    DeltaTempr := {$IFDEF  VER320+}Vodopad.{$ENDIF}CustomIniHelpers.ReadFloat(f, 'Chamber', 'DeltaTempr', 0.7, vPMLocalFormatSettings);
    DeltaTime:= EnsureRange(f.ReadInteger('Chamber', 'DeltaTime', 100), 1, 1000000);
    TimeOutCoeff:= EnsureRange(f.ReadInteger('Chamber', 'TimeOutCoeff', 60)*60, 1, 1000000);
  finally
    FreeAndNil(f);
  end;
end;
end.
