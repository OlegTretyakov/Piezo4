unit ST221MCInterface;

interface
  uses System.Classes;
   type
   TchLigthState = (lOff, lWatch, lExposure);
   IST221MC = interface(IInterface)
     ['{C4755916-7F2F-4D4C-B5F2-B19F20A01E90}']
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
   TFormCreateFunct = function(AOwner : TComponent; ASetTemprBtnClick, ATemperatOffBtnClick : TNotifyEvent):TComponent; stdcall;
implementation

end.
