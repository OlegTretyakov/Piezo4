unit AbstractSearchObject;

interface
  uses Winapi.Windows, Winapi.Messages;

  type
  TAbstractSearchObject= class(TObject)
   public
    Port : byte;
    FormHandle : HWND;
    constructor Create(APort : byte;
                AFormHandle : HWND);virtual;
  end;
  TSearchObjectClass = class of TAbstractSearchObject;

const
C_ChamberSearchState = WM_User + 300;
implementation

constructor TAbstractSearchObject.Create(APort : byte;
                AFormHandle : HWND);
begin
  Port := APort;
  FormHandle := AFormHandle;
end;

end.
