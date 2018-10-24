unit rftdmoduleinterface;

interface
uses DeviceModuleInterface;

type
  TStabMode = (mChallenge, mCharge, mInRange,
  mInRangeTime, mWatch, mOutTarget, mOutTargetInRangeTime, mChargeTimeOut);
  IRftdModule = interface(IDeviceModule)
   ['{40D6B649-E459-4DE2-B42A-8E8DA6E2AC28}'] 
    function GetStabMode: TStabMode; stdcall;  
    function GetStabilizedTime : TDateTime; stdcall;
    function GetTargetCurrent: double; stdcall;
    function GetTargetTarget: double; stdcall;
    procedure SetTemprTarget(const Value: double); stdcall;
    function GetTemprSpeed : double; stdcall;
    procedure SetTemprSpeed(const Value: double); stdcall;
    function GetRegulatorActive: boolean;  stdcall;
    procedure SetRegulatorActive(const Value: boolean); stdcall;
    property RegulatorActive : boolean read GetRegulatorActive write SetRegulatorActive; 
    property StabMode : TStabMode read GetStabMode;      
    property StabilizedTime : TDateTime read GetStabilizedTime;
    property TemprCurrent: double read GetTargetCurrent;
    property TemprTarget : double read GetTargetTarget write SetTemprTarget;
    property TemprSpeed : double read GetTemprSpeed write SetTemprSpeed;
  end;

const
C_ID : Word = 1;
C_Ver : Word = 1;
C_OnRegulatorStateChange : TGUID = '{22990DB6-60B6-4404-82F5-A7C9099EA5BB}';
C_OnTempr : TGUID = '{D20E5F48-2F40-47A9-A7D6-C8DEBEFAA717}';
C_OnTemprCharge : TGUID = '{B40C2921-90F9-4BFB-A474-59F5F36782EB}';
C_OnTemprInTarget : TGUID = '{BF461A4E-0E42-4125-9D7D-286CA35F961D}';
C_OnChargeTimeOut : TGUID = '{2ECB881C-8095-4F83-AD88-D435EB8198B4}';
C_OnTemprOutOfTarget : TGUID = '{B6729A55-ABEC-4905-8983-FFCC447B4655}';
C_OnTemprTargetRet : TGUID = '{6CDE2989-A8E0-4DB8-86B2-0A74C13CB2F8}';

implementation

end.
