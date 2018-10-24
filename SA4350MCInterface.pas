unit SA4350MCInterface;

interface
  uses System.Classes;
  type
  TchLigthState = (lOff, lWatch, lExposure);
   ISA4350MC = interface(IInterface)
    ['{7B76E860-76CC-44E8-A859-57151B65B2CB}']
    procedure BlockForm(ADoBlock : boolean); stdcall;
    procedure SetMessageText(PanelIndex : byte; const AText : string);stdcall;
    procedure SetLigthState(AState: TchLigthState); stdcall;
    procedure UpdateClock(AThen : TDateTime); stdcall;
    procedure SetTargetTempr(const Value : Double);stdcall;
    procedure SetCurrentTempr(const Value : Double);stdcall;
    procedure SetSpeed(const Value : Double);stdcall;
    function GetTargetTempr : Double; stdcall;
    function GetTargetTime : Cardinal; stdcall;
    procedure Close;stdcall;
   end;
   TFormCreateFunct = function(AOwner : TComponent;
                               ASetTemprBtnClick,
                               ATemperatOffBtnClick,
                               AChamberOffBtnClick : TNotifyEvent):TComponent; stdcall;
implementation

end.
